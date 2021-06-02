module Kv where

import Control.Monad
import Data.Either
import Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import Data.Maybe
import Polysemy
import Polysemy.AtomicState

data KVStore k v m a where
  Kvset :: k -> v -> KVStore k v m ()
  Kvget :: k -> KVStore k v m (Maybe v)
  Kvdel :: k -> KVStore k v m ()

makeSem ''KVStore -- template haskell

-- we use atomic operations to be thread safe in concurrency
runKVStoreAtomic ::
  ( Member (AtomicState (HashMap k v)) r,
    Eq k,
    Hashable k
  ) =>
  Sem (KVStore k v ': r) a ->
  Sem r a
runKVStoreAtomic = interpret $ \case
  Kvset k v -> atomicModify' $ Map.insert k v
  Kvget k -> atomicGets $ Map.lookup k
  Kvdel k -> atomicModify' $ Map.delete k

kvgets ::
  (Member (KVStore k v) r, Eq k, Hashable k) =>
  (v -> a) ->
  k ->
  Sem r (Maybe a)
kvgets f k = fmap f <$> kvget k
