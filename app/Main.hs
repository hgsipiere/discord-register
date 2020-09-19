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
import Data.Text.Lazy as TL (Text, foldl')

import qualified Dhall (input, auto, FromDhall)

import qualified Di (new)
import DiPolysemy (runDiToIO)

import GHC.Generics

import qualified Polysemy as P
import qualified Polysemy.AtomicState as P

data Settings = Settings {
  botToken :: Text, botID :: Snowflake User, adminID :: Snowflake User, vchannelID :: Snowflake Channel,
  joinMsg :: Text, nameReciMsg :: Text, emailReciMsg :: Text, screenshotReciMsg :: Text,
  noAtEmailMsg :: Text, noScreenshotMsg :: Text, finishedMsg :: Text
} deriving (Generic, Show)

instance Dhall.FromDhall (Snowflake a)
instance Dhall.FromDhall Settings

data Form = Named Text | NamedEmailed Text Text | Finished

-- TODO split this into multiple functions
-- prelimary work of using generic lens is mostly done
messageCreateAction cfg = \msg@Message{author, M.content = response} ->
-- the bot responding to its messages counts as an event, so ignore that to prevent looping
  case author /= cfg ^. #botID && isNothing (msg ^. #guildID) of
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
            let embedImage = EmbedImage (x ^. #url) (x ^. #proxyUrl) 0 0
            let embed = Embed (Just "sc") (Just "image") (Just "something") Nothing Nothing Nothing Nothing (Just embedImage) Nothing Nothing Nothing Nothing []
            void $ tell (cfg ^. #vchannelID) (name <> " " <> email)
            void $ tell (cfg ^. #vchannelID) embed
            void $ tell (cfg ^. #vchannelID) (x ^. #url)
            void $ tell @Text author $ cfg ^. #screenshotReciMsg
          [] -> void $ tell @Text author $ cfg ^. #noScreenshotMsg
        Just Finished -> void $ tell author $ cfg ^. #finishedMsg

embedEarth = EmbedImage "https://upload.wikimedia.org/wikipedia/commons/9/97/The_Earth_seen_from_Apollo_17.jpg"
                        "https://upload.wikimedia.org/wikipedia/commons/9/97/The_Earth_seen_from_Apollo_17.jpg"
                        3002 3000
embed' = Embed (Just "sc") (Just "image") (Just "something") Nothing Nothing Nothing Nothing (Just embedEarth) Nothing Nothing Nothing Nothing []
main :: IO ()
main = do
  cfg <- Dhall.input Dhall.auto "./config.dhall" :: IO Settings
  let program = do
        react @'GuildMemberAddEvt \ctx -> void $ tell ctx $ cfg ^. #joinMsg
        react @'MessageCreateEvt $ messageCreateAction cfg
        void $ tell (cfg ^. #vchannelID) embed'
  Di.new \di -> do
    void . P.runFinal . P.embedToFinal. runCacheInMemory . runDiToIO di. runMetricsNoop.
      -- if run without IO, the state is localised, ignore final state via fmap
      fmap snd . P.atomicStateToIO Map.empty . runBotIO (BotToken $ cfg ^. #botToken) $ program
