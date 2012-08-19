{-# LANGUAGE StandaloneDeriving #-}
module QuadTree(CachedTree, atPosWithDef, readTree, treeShow,
                treeUpdate, NodeCacheM, initTreeCache, runNodeCacheM)
    where

import qualified Data.Map as M
import Pos
import Data.List
import Control.Monad.State

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
getSubNode pos _ = error $ "Wrong pos in getSubPos : " ++ show pos

setSubNode :: Pos -> b -> Node a b -> Node a b
setSubNode (Pos 0 0) x (Node _  v2 v3 v4) = Node x  v2 v3 v4
setSubNode (Pos 0 1) x (Node v2 _  v3 v4) = Node v2 x  v3 v4
setSubNode (Pos 1 0) x (Node v2 v3 _  v4) = Node v2 v3 x  v4
setSubNode (Pos 1 1) x (Node v2 v3 v4 _ ) = Node v2 v3 v4 x

atPosWithDef :: CachedTree a -> a -> Pos -> a
atPosWithDef (In (CachedNode _ _ Void)) def _ = def
atPosWithDef (In (CachedNode _ 1 (Leaf l))) _ (Pos 0 0) = l
atPosWithDef (In (CachedNode _ sz node@Node{})) def pos@(Pos x y) | x >= sz || y >= sz =
                                                                  error $ "Wrong at pos : " ++ show pos ++ "size is only " ++ show sz
atPosWithDef (In (CachedNode _ sz node@Node{})) def pos = 
    let middle = sz `div` 2
        subPos = mapPos (`div` middle) pos
        offsPos = pos - subPos * (Pos middle middle)
    in atPosWithDef (getSubNode subPos node) def offsPos

type NodeCache a = M.Map (NodeCacheKey a) (CachedTree a)

nodeCache :: Ord a => Node a (CachedTree a) -> NodeCacheKey a
nodeCache Void = NodeCacheKey Void
nodeCache (Leaf l) = NodeCacheKey $ Leaf l
nodeCache node = NodeCacheKey $ fmap (nodeIdx . out) node

readNodes :: [[a]] -> Int -> CachedTree a
readNodes ((v : vs) : rows) 1 = In $ CachedNode 0 1 $ Leaf v
readNodes ([]:rows) 1 = In $ CachedNode 0 1 $ Void
readNodes rows sz | all null rows = In $ CachedNode  0 sz Void
readNodes rows sz = 
    let middle = sz `div` 2
        bottomRows = drop middle rows
        topRight = map (drop middle) rows
        bottomRight = map (drop middle) bottomRows
    in In $ CachedNode 0 sz $ fmap (\ lst -> readNodes lst middle)
                      (Node rows bottomRows topRight bottomRight)

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
    zipWith (++) (treeShow v1) (treeShow v3) ++
    zipWith (++) (treeShow v2) (treeShow v4)

type NodeCacheM a = StateT (NodeCacheState a) 

data NodeCacheState a = NodeCacheState { ncCache :: (NodeCache a),
                                         ncCurIdx :: NodeIdx }

newNode :: (Ord a, Monad m) => Node a (CachedTree a) -> Int -> NodeCacheM a m (CachedTree a)
newNode node sz = return $ In $ CachedNode 0 sz node
-- newNode node sz = 
--     do 
--       let cacheVal = nodeCache node
--       tab <- gets ncCache
--       case M.lookup cacheVal tab of
--         Just v -> return v
--         Nothing -> do
--           idx <- gets ncCurIdx
--           let nodeVal = In $ CachedNode idx sz node
--           modify $ \ (NodeCacheState cache idx) -> 
--               NodeCacheState (M.insert cacheVal nodeVal cache)
--                              (idx + 1)
--           return nodeVal

treeUpdate :: (Ord a, Monad m) => CachedTree a -> Pos -> a -> NodeCacheM a m (CachedTree a)
treeUpdate (In (CachedNode _ sz Void)) pos val | sz == 1 =
    newNode (Leaf val) sz
treeUpdate (In (CachedNode _ sz Void)) pos val = do
    voidNode <- newNode Void (sz `div` 2)
    fourVoids <- newNode (Node voidNode voidNode voidNode voidNode) sz
    treeUpdate fourVoids pos val
treeUpdate (In (CachedNode _ sz (Leaf l))) pos val =
    newNode (Leaf val) sz
treeUpdate (In (CachedNode _ sz node@Node{})) pos val = 
    let middle = sz `div` 2
        subPos =  mapPos (`div` middle) pos
        offsPos = pos - subPos * (Pos middle middle)
    in 
      do subNode <- treeUpdate (getSubNode subPos node) offsPos val
         let nodeWithSub = setSubNode subPos subNode node
         newNode nodeWithSub sz

initTreeCache :: (Ord a, Monad m) => CachedTree a -> NodeCacheM a m (CachedTree a)
initTreeCache (In (CachedNode _ sz Void)) = newNode Void sz
initTreeCache (In (CachedNode _ sz (Leaf l))) = newNode (Leaf l) sz
initTreeCache (In (CachedNode _ sz (Node n1 n2 n3 n4))) = do
   n1' <- initTreeCache n1
   n2' <- initTreeCache n2
   n3' <- initTreeCache n3
   n4' <- initTreeCache n4
   newNode (Node n1' n2' n3' n4') sz

runNodeCacheM :: (Ord a, Monad m) => NodeCacheM a m r -> m r
runNodeCacheM comp = evalStateT comp (NodeCacheState M.empty 0) 
