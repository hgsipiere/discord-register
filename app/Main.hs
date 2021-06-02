module Main where

import Calamity
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands (useConstantPrefix)
import Calamity.Commands.Context (useLightContext)
import Calamity.Metrics.Noop (runMetricsNoop)
import Control.Lens
import Control.Monad (void)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString (readFile)
import Data.Flags (allFlags)
import qualified Data.HashMap.Strict as Map
import qualified Di (new)
import DiPolysemy (runDiToIO)
import GHC.Conc (newTVarIO)
import Kv
import Lib
import Polysemy
import Polysemy.AtomicState
import Prelude hiding (readFile)

main = do
  eitherDecodeFileStrict "./config.json" >>= \case
    Left e -> print e
    Right cfg -> do
      emptyMap <- newTVarIO (Map.empty :: Map.HashMap (Snowflake User) Form)
      Di.new \di -> -- logging continuation? this is strange
        do
          void . runFinal . embedToFinal . runCacheInMemory
          . runDiToIO di -- logging
          . runMetricsNoop -- no metrics
          . useConstantPrefix "¬"
          . runAtomicStateTVar emptyMap -- trying not to pollute IO
          . runKVStoreAtomic
          . useLightContext -- light command context
          -- all intents
          . runBotIO (BotToken $ cfg ^. #botToken) allFlags
          $ program cfg
