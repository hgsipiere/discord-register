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
  joinMsg :: Text, nameRecievedMsg :: Text,
  emailRecievedMsg :: Text, noAtEmailMsg :: Text, finishedMsg :: Text
} deriving (Generic, Show)

instance Dhall.FromDhall (Snowflake a)
instance Dhall.FromDhall Settings

data Form = Named Text | Finished

main :: IO ()
main = do
  cfg <- Dhall.input Dhall.auto "./config.dhall" :: IO Settings
  let program = do
        react @'GuildMemberAddEvt \ctx -> void $ tell ctx $ cfg ^. #joinMsg

        react @'MessageCreateEvt \ctx@Message{author, M.content = response} ->
          -- the bot responding to its messages counts as an event, so ignore that to prevent looping
          case author /= cfg ^. #botID && isNothing (ctx ^. #guildID) of
            False -> pure ()
            True -> do
              current <- P.atomicGet
              case Map.lookup author current of
                Nothing -> do
                  _ <- P.atomicPut $ Map.insert author (Named response) current
                  void $ tell author $ cfg ^. #nameRecievedMsg
                Just (Named name) -> case TL.foldl' (\t c -> t || c == '@') False response of
                  False -> void $ tell author $ cfg ^. #noAtEmailMsg
                  True -> do
                    _ <- P.atomicPut $ Map.insert author Finished current
                    void $ tell (cfg ^. #vchannelID) (name <> " " <> response)
                    void $ tell author $ cfg ^. #emailRecievedMsg
                Just Finished -> void $ tell author $ cfg ^. #finishedMsg
  Di.new \di -> do
    void . P.runFinal . P.embedToFinal. runCacheInMemory . runDiToIO di. runMetricsNoop.
      -- if run without IO, the state is localised, ignore final state via fmap
      fmap snd . P.atomicStateToIO Map.empty . runBotIO (BotToken $ cfg ^. #botToken) $ program
