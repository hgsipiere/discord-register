module Main where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Metrics.Noop
import qualified Calamity.HTTP.Guild
import qualified Calamity.Types.Model.Channel.Message as M

import Control.Lens ((^.), makeLenses)
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
  _botToken :: Text, _botID :: Snowflake User, _adminID :: Snowflake User, _vchannelID :: Snowflake Channel,
  _joinMsg :: Text, _nameRecievedMsg :: Text,
  _emailRecievedMsg :: Text, _noAtEmailMsg :: Text, _finishedMsg :: Text
} deriving (Generic, Show)

makeLenses ''Settings

instance Dhall.FromDhall (Snowflake a)
instance Dhall.FromDhall Settings

data Form = Named Text | Finished

emptyUserFormMap :: Map.HashMap (Snowflake User) Form
emptyUserFormMap = Map.empty

main :: IO ()
main = do
  cfg <- (Dhall.input Dhall.auto "./config.dhall" :: IO Settings)
  let program = do
        react @'GuildMemberAddEvt $ \ctx -> do
          void $ tell ctx $ cfg ^. joinMsg

        react @'MessageCreateEvt $ \ctx@M.Message{M.author=writer, M.guildID = guildIDOpt, M.content = response} -> do
          -- the bot responding to its messages counts as an event, so ignore that to prevent looping
          case (writer /= cfg ^. botID) && isNothing guildIDOpt of
            False -> pure ()
            True -> do
              current <- P.atomicGet
              case Map.lookup writer current of
                Nothing -> do
                  _ <- P.atomicPut $ Map.insert writer (Named response) current
                  void $ tell writer $ cfg ^. nameRecievedMsg
                Just (Named name) -> case TL.foldl' (\t c -> t || c == '@') False response of
                  False -> void $ tell writer $ cfg ^. noAtEmailMsg
                  True -> do
                    _ <- P.atomicPut (Map.insert writer Finished current)
                    void $ tell (cfg ^. vchannelID) (name <> " " <> response)
                    void $ tell writer $ cfg ^. emailRecievedMsg
                Just Finished -> void $ tell writer $ cfg ^. finishedMsg
  Di.new \di -> do
    void . P.runFinal . P.embedToFinal. runCacheInMemory . runDiToIO di. runMetricsNoop.
      -- if run without IO, the state is localised, ignore final state via fmap
      fmap snd . P.atomicStateToIO emptyUserFormMap . runBotIO (BotToken $ cfg ^. botToken) $ program
