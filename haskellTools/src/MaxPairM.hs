module MaxPairM
  ( maxPairM
  ) where

getGreater :: Ord a => (a, a) -> a
getGreater (a0, a1) = if a0 > a1
  then a0
  else a1

maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM pair = pair >>= (\p -> return (getGreater p))

