module Main where

import Calamity hiding (author)
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Metrics.Noop
import qualified Calamity.HTTP.Guild
import Calamity.Types.Model.Channel.Message as M

import Data.Generics.Internal.VL ((^.))
import Control.Monad (void)

import qualified Data.HashMap.Strict as Map (insert, empty, lookup, HashMap)
import Data.Maybe (isNothing)
import Data.Text.Lazy as TL (Text, foldl', append, any, singleton)

import qualified Dhall (input, auto, FromDhall)

import qualified Di (new)
import DiPolysemy (runDiToIO, info)

import GHC.Generics

import qualified Polysemy as P
import qualified Polysemy.AtomicState as P

import TextShow
data Settings = Settings {
  botToken :: Text, botID :: Snowflake User, adminID :: Snowflake User, vchannelID :: Snowflake Channel,
  joinMsg :: Text, nameReciMsg :: Text, emailReciMsg :: Text, screenshotReciMsg :: Text,
  noAtEmailMsg :: Text, noScreenshotMsg :: Text, finishedMsg :: Text,
  infoMsg :: Text
} deriving (Generic, Show)

instance Dhall.FromDhall (Snowflake a)
instance Dhall.FromDhall Settings
data Form = Named Text | NamedEmailed Text Text | Finished

-- TODO split this into multiple functions
-- prelimary work of using generic lens is mostly done
messageCreateAction cfg = \msg@Message{author, M.content = response} -> do
-- the bot responding to its messages counts as an event, so ignore that to prevent looping
  case author /= cfg ^. #botID && isNothing (msg ^. #guildID) && (not $ TL.any (== '¬') response) of
    False -> pure ()
    True -> do
      current <- P.atomicGet
      case Map.lookup author current of
        Nothing -> do
          _ <- P.atomicPut $ Map.insert author (Named response) current
          void $ tell author $ cfg ^. #nameReciMsg
        Just (Named name) -> case TL.foldl' (\t c -> t || c == '@') False response of
          False -> void $ tell author $ cfg ^. #noAtEmailMsg
          True -> do
            _ <- P.atomicPut $ Map.insert author (NamedEmailed name response) current
            void $ tell author $ cfg ^. #emailReciMsg
        Just (NamedEmailed name email) -> case msg ^. #attachments of
          files@(x:_) -> do
	    _ <- P.atomicPut $ Map.insert author Finished current
            void $ tell (cfg ^. #vchannelID) (name <> " " <> email)
            void $ tell (cfg ^. #vchannelID) (x ^. #url)
	    void $ tell (cfg ^. #vchannelID) (toMention author)
            void $ tell @Text author $ cfg ^. #screenshotReciMsg
          [] -> void $ tell @Text author $ cfg ^. #noScreenshotMsg
        Just Finished -> do
	  void $ tell author $ cfg ^. #finishedMsg

toMention :: Snowflake User -> CreateMessageOptions
toMention sUser = CreateMessageOptions (Just $ showt ("<@" `append`  showtl sUser `append` ">"))
  Nothing Nothing Nothing Nothing -- no nonce, tts, file, embed
  (Just (AllowedMentions [AllowedMentionUsers] [] [sUser]))

main :: IO ()
main = do
  cfg <- Dhall.input Dhall.auto "./config.dhall" :: IO Settings
  let program = do
        react @'GuildMemberAddEvt \ctx -> void $ tell ctx $ cfg ^. #joinMsg
        react @'MessageCreateEvt $ messageCreateAction cfg
        addCommands ( do
	  helpCommand
	  command @'[] "info" \ctx -> void $ tell @Text ctx $ cfg ^. #infoMsg)
  Di.new \di -> do
    void . P.runFinal . P.embedToFinal. runCacheInMemory . runDiToIO di. runMetricsNoop. useConstantPrefix "¬" .
      -- if run without IO, the state is localised, ignore final state via fmap
      fmap snd . P.atomicStateToIO Map.empty . runBotIO (BotToken $ cfg ^. #botToken) $ program
