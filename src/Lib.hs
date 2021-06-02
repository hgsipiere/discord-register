{-# LANGUAGE TypeApplications #-}

module Lib where

import Calamity as Cal hiding (Embed, Member, author, embed)
import Calamity.Commands (addCommands, command, helpCommand)
import Calamity.Commands.Context (ctxUserID)
import qualified Calamity.HTTP.Channel as HttpChan
import qualified Calamity.HTTP.Guild
import qualified Calamity.Types.Model.Channel.Attachment as Attachm
import Calamity.Types.Model.Channel.Message as Messg
import Calamity.Types.Upgradeable (upgrade)
import Control.Lens
import Control.Monad (mapM_, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (foldl')
import Data.Maybe (isJust)
import qualified Data.Text as TS
import Data.Text.Lazy as TL (Text, any, append, intercalate, pack, unpack, unwords)
import Data.Text.Lazy.IO (writeFile)
import qualified Data.Vector.Unboxing as V (elem)
import GHC.Generics (Generic)
import Kv
import Parser (parseFullName) -- get rid of this
import Polysemy
import Text.Regex.TDFA ((=~))
import Utils (EssentialEnv)
import Prelude hiding (any, intercalate, readFile, unwords, writeFile)

data InfoCmd = InfoCmd {cmd_name :: TS.Text, info :: Text} deriving (Generic)

data Query = Query {incorrect_response :: Text, correct_response :: Text} deriving (Generic)

data Cfg = Cfg
  { botToken :: Text,
    botID :: Snowflake User,
    vChannelID :: Snowflake Channel,
    joinMsg :: Text,
    infoCommands :: [InfoCmd],
    name_query :: Query,
    screenshot_query :: Query,
    done :: Text
  }
  deriving (Generic)

-- this allows us to use a configuration file
instance FromJSON Query

instance FromJSON InfoCmd

instance FromJSON Cfg

data Form = NoDetails | Named Text | Finished

-- proposition that a value is a program with key value store
type Env r c =
  ( EssentialEnv r c,
    Member (KVStore (Snowflake User) Form) r
  )

messageCreateAction :: Env r c => Cfg -> Message -> Sem r ()
messageCreateAction cfg msg@Message {author, Messg.content = response, attachments} =
  -- check the bot would be replying to itself or a non-personal message
  -- this should also avoid replying to a command too, todo later
  if author == cfg ^. #botID || isJust (msg ^. #guildID)
    then pure ()
    else
      kvget author >>= \case
        Nothing -> grabName cfg author response -- nothing stored about them
        Just NoDetails -> grabName cfg author response -- they gave no details
        Just (Named name) -> grabScreenshot cfg author name attachments
        Just Finished -> void . tell author $ cfg ^. #done

grabName :: Env r c => Cfg -> Snowflake User -> Text -> Sem r ()
-- this should parse inline so we can remove megaparsec as dependency
grabName cfg author response = case parseFullName response of
  Left _ -> void $ tell author $ cfg ^. #name_query ^. #incorrect_response
  Right name -> do
    kvset author (Named name)
    void $ tell author $ cfg ^. #name_query ^. #correct_response

grabScreenshot ::
  Env r c =>
  Cfg ->
  Snowflake User ->
  Text ->
  [Attachm.Attachment] ->
  Sem r ()
grabScreenshot cfg author _ [] =
  void . tell author $ cfg ^. #screenshot_query ^. #incorrect_response
grabScreenshot cfg author name (file : _) = do
  kvset author Finished
  tell author $ cfg ^. #screenshot_query ^. #correct_response
  void . tell (cfg ^. #vChannelID) $
    "Discord ID: " <> mention author <> "\n"
      <> "Name: "
      <> name
      <> "\n"
      <> "Attachment: "
      <> file ^. #url

addedCommands :: Env r c => Cfg -> Sem r ()
addedCommands cfg = void . addCommands $ do
  helpCommand
  mapM_ -- add all the information commands, e.g. info, privacy etc...
    (\(InfoCmd cmdName info) -> command @'[] cmdName $ \ctx -> void $ tell ctx info)
    $ cfg ^. #infoCommands

program :: Env r c => Cfg -> Sem r ()
program cfg = do
  addedCommands cfg
  react @'GuildMemberAddEvt $ \ctx -> void . tell ctx $ cfg ^. #joinMsg
  void . react @'MessageCreateEvt $ messageCreateAction cfg
