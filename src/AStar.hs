{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module AStar
    where

import qualified Data.Heap as H
import qualified Data.Set as S

import Data.List
import Data.Function

type Estimate = Int

class (Ord a, Monad m) => SearchState a m | a -> m where
    nextStates :: a -> m [a]
    report :: a -> Int -> m ()
    estimate :: a -> Estimate
    finalState :: a -> Bool
    deadState :: a -> Bool

newtype HeapElem a = HeapElem { getElem :: a }

instance SearchState a m => Eq (HeapElem a) where
    (==) = (==) `on` (estimate . getElem)

instance SearchState a m => Ord (HeapElem a) where
    compare = compare `on` (estimate . getElem)

aStarIters :: (SearchState a m) => Int -> H.Heap (HeapElem a) -> S.Set a -> m (Maybe a)
aStarIters counter queue deadStateSet
    | H.null queue = return Nothing
    | True = let (Just (HeapElem state, newQueue)) = H.uncons queue
             in do report state counter
                   if (state `S.member` deadStateSet) || deadState state
                     then aStarIters (counter + 1) newQueue deadStateSet
                     else if finalState state 
                            then return $ Just state
                            else do newStates <- nextStates state
                                    let queue' = foldl' (\ h p -> H.insert p h) newQueue $ map HeapElem newStates
                                    aStarIters (counter + 1) queue' (S.insert state deadStateSet)

aStar :: (SearchState a m) => a -> m (Maybe a)
aStar initState = aStarIters 0 (H.singleton $ HeapElem initState) S.empty