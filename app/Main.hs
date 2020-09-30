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
import qualified Data.Text as T
import Data.Text.Lazy as TL (Text, pack, intercalate, unpack, foldl', append, any, singleton)

import qualified Dhall (input, auto, FromDhall)

import qualified Di (new)
import DiPolysemy (runDiToIO, info)

import GHC.Generics

import qualified Polysemy as P
import qualified Polysemy.AtomicState as P

import TextShow

import Text.Megaparsec.Byte (space, string)

import qualified Data.Vector.Unboxing as V (elem)

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as CL

if' True a _ = a
if' False _ b = b

type ParserStr = Parsec Void String

preLexeme, lexeme :: ParserStr a -> ParserStr a
lexeme = CL.lexeme C.space
preLexeme = (*>) C.space

nameOrUnderscore', fullName :: ParserStr String
nameOrUnderscore' = (some letterChar) <|> (fmap pure . char $ '_')
fullName = do
  firstName <- preLexeme nameOrUnderscore'
  restNames <- lexeme . some . preLexeme $ nameOrUnderscore'
  pure $ foldl (\z s -> z ++ " " ++ s) firstName restNames

data Settings = Settings {
  botToken :: Text, botID :: Snowflake User, adminID :: Snowflake User, vchannelID :: Snowflake Channel,
  adminRoleID :: Snowflake Role,
  joinMsg :: Text, faultyNameReciMsg :: Text, nameReciMsg :: Text, emailReciMsg :: Text, screenshotReciMsg :: Text,
  noAtEmailMsg :: Text, noScreenshotMsg :: Text, finishedMsg :: Text,
  privacyMsg :: Text, infoMsg :: Text,
  iveBeenResetMsg :: Text, resetNeedsGuildMsg :: Text, notAdminMsg :: Text
} deriving (Generic, Show)

instance Dhall.FromDhall (Snowflake a)
instance Dhall.FromDhall Settings
data Form = Empty | Named Text | NamedEmailed Text Text | Finished

mentionUser :: Snowflake User -> Text -> Text -> CreateMessageOptions
mentionUser sUser preMention content = CreateMessageOptions
  (Just . T.pack . TL.unpack $
    preMention `append` "<@" `append`  showtl sUser `append` ">" `append` content)
  Nothing Nothing Nothing Nothing -- no nonce, tts, file, embed
  (Just (AllowedMentions [AllowedMentionUsers] [] [sUser]))

grabName cfg msg = 
  case runParser fullName "" (unpack response) of
  Left error -> void $ tell author $ cfg ^. #faultyNameReciMsg
  Right name -> do
      current <- P.atomicGet
      _ <- P.atomicPut $ Map.insert author (Named $ showtl name) current
      void $ tell author $ cfg ^. #nameReciMsg
  where author = msg ^. #author
        response = msg ^. #content

grabEmail cfg name msg =
  case (TL.foldl' (\n c -> if' (c == '@') (n+1) n) 0 response) == 1 of
    False -> void $ tell author $ cfg ^. #noAtEmailMsg
    True -> do
      current <- P.atomicGet
      _ <- P.atomicPut $ Map.insert author (NamedEmailed name response) current
      void $ tell author $ cfg ^. #emailReciMsg
  where response = msg ^. #content
        author = msg ^. #author

grabScreenshot cfg name email msg =
  let author = msg ^. #author in
  case msg ^. #attachments of
    files@(x:_) -> do
      current <- P.atomicGet
      _ <- P.atomicPut $ Map.insert author Finished current
      void . tell (cfg ^. #vchannelID) . mentionUser author "Discord ID: " $
        "\nName: " <> name <> "\nEmail: " <> email <> "\nAttachment: " <> (x ^. #url)
      void $ tell @Text author $ cfg ^. #screenshotReciMsg
    [] -> void $ tell @Text author $ cfg ^. #noScreenshotMsg

-- the bot responding to its messages counts as an event, so ignore that to prevent looping
messageCreateAction cfg = \msg@Message{author, M.content = response} -> do
  case author /= cfg ^. #botID && isNothing (msg ^. #guildID) && (not $ TL.any (== '¬') response) of
    False -> pure ()
    True -> do
      current <- P.atomicGet
      case Map.lookup author current of
        Nothing -> grabName cfg msg
        Just Empty -> grabName cfg msg
        Just (Named name) -> grabEmail cfg name msg
        Just (NamedEmailed name email) -> grabScreenshot cfg name email msg
        Just Finished -> void $ tell author $ cfg ^. #finishedMsg

resetCommand cfg = command @'[Snowflake User, [Text]] "reset" \ctx sUser reason ->
  case ctx ^. #member of
    Nothing -> void $ tell @Text ctx $ cfg ^. #resetNeedsGuildMsg
    Just mem -> if V.elem (cfg ^. #adminRoleID) (mem ^. #roles) then
      do
        current <- P.atomicGet
        _ <- P.atomicPut $ Map.insert sUser Empty current
        void $ tell @Text sUser $ TL.intercalate " " reason
        void $ tell sUser $ cfg ^. #iveBeenResetMsg
      else void $ tell ctx $ cfg ^. #notAdminMsg

commandsAdded cfg = addCommands $ do
  helpCommand
  command @'[] "info" \ctx -> void $ tell ctx $ cfg ^. #infoMsg
  command @'[] "privacy" \ctx -> void $ tell ctx $ cfg ^. #privacyMsg
  resetCommand cfg

program cfg = 
  (react @'GuildMemberAddEvt \ctx -> void $ tell ctx $ cfg ^. #joinMsg) >>
  (react @'MessageCreateEvt $ messageCreateAction cfg) >>
  commandsAdded cfg

main :: IO ()
main = do
  cfg <- Dhall.input Dhall.auto "./config.dhall" :: IO Settings
  Di.new \di -> do
    void . P.runFinal . P.embedToFinal. runCacheInMemory . runDiToIO di. runMetricsNoop. useConstantPrefix "¬" .
      -- if run without IO, the state is localised, ignore final state via fmap
      fmap snd . P.atomicStateToIO Map.empty . runBotIO (BotToken $ cfg ^. #botToken) $ program cfg
