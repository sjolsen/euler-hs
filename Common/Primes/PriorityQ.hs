module Common.Primes.PriorityQ where

import qualified Data.PQueue.Min as PQBase

data KV k v = KV {key :: k, value :: v}

asPair :: KV k v -> (k, v)
asPair (KV k v) = (k, v)

instance Ord k => Ord (KV k v) where
  compare (KV j _) (KV k _) = compare j k

instance Eq k => Eq (KV k v) where
  (KV j _) == (KV k _) = j == k

type PriorityQ k v = PQBase.MinQueue (KV k v)

empty :: PriorityQ k v
empty = PQBase.empty

minKey :: PriorityQ k v -> k
minKey = key . PQBase.findMin

minKeyValue :: PriorityQ k v -> (k, v)
minKeyValue = asPair . PQBase.findMin

insert :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v
insert k v = PQBase.insert (KV k v)

deleteMinAndInsert :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v
deleteMinAndInsert k v = insert k v . PQBase.deleteMin
