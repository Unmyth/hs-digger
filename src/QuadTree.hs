{-# LANGUAGE StandaloneDeriving #-}
module QuadTree
    where

import qualified Data.Map as M
import Pos
import Data.List

data Node a b = Void
              | Leaf !a 
              | Node !b !b !b !b

instance Functor (Node a) where
    fmap f Void = Void
    fmap f (Leaf x) = Leaf x
    fmap f (Node v1 v2 v3 v4) = Node (f v1) (f v2) (f v3) (f v4)
                
newtype Mu f = In {out :: (f (Mu f))}

type NodeIdx = Int

data CachedNode a b = CachedNode { nodeIdx :: !NodeIdx,
                                   nodeSize :: !Int,
                                   nodeData :: !(Node a b) }

type CachedTree a = Mu (CachedNode a)

newtype NodeCacheKey a = NodeCacheKey (Node a NodeIdx)

deriving instance (Eq a, Eq b) => Eq (Node a b)
deriving instance (Ord a, Ord b) => Ord (Node a b)

deriving instance Eq a => Eq (NodeCacheKey a)
deriving instance Ord a => Ord (NodeCacheKey a)

getSubNode :: Pos -> Node a b -> b
getSubNode (Pos 0 0) (Node x _ _ _) = x
getSubNode (Pos 0 1) (Node _ x _ _) = x
getSubNode (Pos 1 0) (Node _ _ x _) = x
getSubNode (Pos 1 1) (Node _ _ _ x) = x

setSubNode :: Pos -> b -> Node a b -> Node a b
setSubNode (Pos 0 0) x (Node _  v2 v3 v4) = Node x  v2 v3 v4
setSubNode (Pos 0 1) x (Node v2 _  v3 v4) = Node v2 x  v3 v4
setSubNode (Pos 1 0) x (Node v2 v3 _  v4) = Node v2 v3 x  v4
setSubNode (Pos 1 1) x (Node v2 v3 v4 _ ) = Node v2 v3 v4 x

atPosWithDef :: CachedTree a -> a -> Pos -> a
atPosWithDef (In (CachedNode _ _ Void)) def _ = def
atPosWithDef (In (CachedNode _ 1 (Leaf l))) _ (Pos 0 0) = l
atPosWithDef (In (CachedNode _ sz node@Node{})) def pos = 
    let middle = sz `div` 2
        subPos = mapPos (`div` middle) pos
        offsPos = pos - subPos * (Pos middle middle)
    in atPosWithDef (getSubNode subPos node) def offsPos

type NodeCache a = M.Map (NodeCacheKey a) NodeIdx

nodeCache :: Ord a => Node a (CachedTree a) -> NodeCacheKey a
nodeCache Void = NodeCacheKey Void
nodeCache (Leaf l) = NodeCacheKey $ Leaf l
nodeCache node = NodeCacheKey $ fmap (nodeIdx . out) node

readNodes :: [[a]] -> Int -> CachedTree a
readNodes ((v : vs) : rows) 1 = In $ CachedNode 0 1 $ Leaf v
readNodes rows sz | all null rows = In $ CachedNode  0 sz Void
readNodes rows sz = 
    let middle = sz `div` 2
        bottomRows = drop middle rows
        topRight = map (drop middle) rows
        bottomRight = map (drop middle) bottomRows
    in In $ CachedNode 0 sz $ fmap (\ lst -> readNodes lst middle)
                      (Node rows topRight bottomRows bottomRight)

readTree :: [[a]] -> CachedTree a
readTree rows = let w = maximum $ map length rows
                    h = length rows
                    pows = unfoldr (\ v -> Just (v, 2 * v)) 1
                    sz = head $ dropWhile (< (max w h)) pows
                in readNodes rows sz

treeShow :: Show a => CachedTree a -> [String]
treeShow (In (CachedNode _ sz Void)) = replicate sz ""
treeShow (In (CachedNode _ 1 (Leaf l))) = [show l]
treeShow (In (CachedNode _ sz (Node v1 v2 v3 v4))) =
    zipWith (++) (treeShow v1) (treeShow v2) ++
    zipWith (++) (treeShow v3) (treeShow v4)    