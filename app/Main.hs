{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Metrics.Noop
import qualified Calamity.HTTP.Guild
import qualified Calamity.Types.Model.Channel.Message as M

import Control.Monad

import qualified Data.HashMap.Strict as Map
import Data.Maybe (isNothing)
import Data.Text.Lazy (Text, pack)

import qualified Dhall

import qualified Di
import DiPolysemy

import GHC.Generics

import qualified Polysemy as P
import qualified Polysemy.AtomicState as P

import System.Environment

data Settings = Settings {token :: Text, botID :: Snowflake User, adminID :: Snowflake User, vchannelID :: Snowflake Channel}
  deriving (Generic, Show)

instance Dhall.FromDhall (Snowflake a)
instance Dhall.FromDhall Settings

data Form = Named Text | Finished

emptyUserFormMap :: Map.HashMap (Snowflake User) Form
emptyUserFormMap = Map.empty

main :: IO ()
main = do
  settings@Settings{token, botID, adminID, vchannelID} <- (Dhall.input Dhall.auto "./config.dhall" :: IO Settings)
  let program = do
        react @'GuildMemberAddEvt $ \ctx -> do
          void $ tell @Text ctx "Please send your full name."

        react @'MessageCreateEvt $ \ctx@M.Message{M.author=writer, M.guildID = guildIDOpt, M.content = response} -> do
          -- the bot responding to its messages counts as an event, so ignore that to prevent looping
          case (writer /= botID) && isNothing guildIDOpt of
            False -> pure ()
            True -> do
              current <- P.atomicGet
              case Map.lookup writer current of
                Nothing -> do
                  _ <- P.atomicPut $ Map.insert writer (Named response) current
                  void $ tell @Text writer "Thanks for name."
                Just (Named name) -> do
                  _ <- P.atomicPut (Map.insert writer Finished current)
                  void $ tell vchannelID (name <> " " <> response)
                  void $ tell @Text writer "Done, sent to verification channel."
                Just Finished -> void $ tell @Text writer "Nothing to do"
  Di.new \di -> do
    void . P.runFinal . P.embedToFinal. runCacheInMemory . runDiToIO di. runMetricsNoop.
      -- if run without IO, the state is localised, ignore final state via fmap
      fmap snd . P.atomicStateToIO emptyUserFormMap .
      useConstantPrefix "!" $ runBotIO (BotToken token) $ program
