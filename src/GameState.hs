module GameState
    where

import Pos
import QuadTree
import GameField
import Control.Monad
import Data.Maybe

import Data.List

import qualified Data.Map as M
import qualified Data.Heap as H
import qualified Data.Foldable as F

type PosHeap = H.Heap Pos

data Status = InProgress | Win | Lost | Aborted
            deriving (Eq, Show)

data GameState = GameState { gsField :: !GameField,
                             gsCurScore :: !Int,
                             gsUpdateList :: !PosHeap,
                             gsStatus :: !Status,
                             gsPath :: ![Command]}

makeGameState :: GameField -> GameState
makeGameState gf = GameState {
                              gsField = gf,
                              gsCurScore = 0,
                              gsUpdateList = addCellsPos H.empty
                                               [Pos x y | x <- [0 .. (gfgWidth $ gfGlobals gf) - 1],
                                                          y <- [0 .. (gfgHeight $ gfGlobals gf) - 1]],
                              gsStatus = InProgress,
                              gsPath = []
                             }

data Command = CmdU | CmdL | CmdR | CmdD | CmdW | CmdA | CmdS
             deriving (Eq, Ord)

instance Show Command where
    show CmdU = "U"
    show CmdL = "L"
    show CmdR = "R"
    show CmdD = "D"
    show CmdW = "W"
    show CmdA = "A"
    show CmdS = "S"

charToCmd :: Char -> Command
charToCmd 'U' = CmdU
charToCmd 'L' = CmdL
charToCmd 'R' = CmdR
charToCmd 'D' = CmdD
charToCmd 'W' = CmdW
charToCmd 'A' = CmdA
charToCmd 'S' = CmdS

manualCharToCmd :: Char -> Command
manualCharToCmd 'w' = CmdU
manualCharToCmd 'a' = CmdL
manualCharToCmd 'd' = CmdR
manualCharToCmd 's' = CmdD
manualCharToCmd ' ' = CmdW
manualCharToCmd 'q' = CmdA
manualCharToCmd 'e' = CmdS

cmdDelta :: Command -> Pos
cmdDelta CmdU = Pos 0 1
cmdDelta CmdD = Pos 0 (-1)
cmdDelta CmdR = Pos 1 0
cmdDelta CmdL = Pos (-1) 0

type GameM m = NodeCacheM Cell m

mapUpdate :: Monad m => FieldMap -> [(Pos, Cell)] -> GameM m FieldMap
mapUpdate m lst = foldM (\ m (p, c) -> treeUpdate m p c) m lst

addCellReq :: PosHeap -> Pos -> PosHeap
addCellReq h pos = foldl' (\ h p -> H.insert p h) h
                     [pos, pos + Pos (-1) 0, pos + Pos 1 0,
                      pos + Pos 0 1, pos + Pos (-1) 1, pos + Pos 1 1]

addCellsReq :: PosHeap -> [Pos] -> PosHeap
addCellsReq h lst = foldl' addCellReq h lst

addCellsPos :: PosHeap -> [Pos] -> PosHeap
addCellsPos h posLst = foldl' (\ h p -> H.insert p h) h posLst

updateRobot :: Monad m => GameState -> Command -> GameM m GameState
updateRobot gs CmdW = return $ gs { gsCurScore = gsCurScore gs - 1 }
updateRobot gs CmdA = return $ gs { gsStatus = Aborted, gsCurScore = gsCurScore gs + (25 * gfCurLambdas (gsField gs)) }
updateRobot gs CmdS = let field = gsField gs
                          fMap = gfMap field
                          rPos = gfRobotPos field
                          beardPosLst = filter (\ p -> atPos fMap p == Beard) [Pos x y | x <- [posX rPos - 1 .. posX rPos + 1],
                                                                                         y <- [posY rPos - 1 .. posY rPos + 1]]
                          newHeap = addCellsReq (gsUpdateList gs) beardPosLst
                      in do
                          newMap <- mapUpdate fMap $ map (\ p -> (p, Empty)) beardPosLst
                          return gs {
                                     gsCurScore = gsCurScore gs - 1,
                                     gsUpdateList = newHeap,
                                     gsField = field {
                                                      gfMap = newMap,
                                                      gfNumBeards = gfNumBeards field - length beardPosLst,
                                                      gfCurRazors = gfCurRazors field - 1
                                                     }}
updateRobot gs cmd = let field = gsField gs
                         fMap = gfMap field
                         rPos = gfRobotPos field
                         newPos = rPos + cmdDelta cmd
                         target = atPos fMap newPos
                         newPos2 = gfRobotPos field + 2 * (cmdDelta cmd)
                     in updateRobot' gs cmd field fMap rPos newPos target newPos2

updateRobot' :: Monad m => GameState -> Command -> GameField -> FieldMap -> Pos -> Pos -> Cell -> Pos -> GameM m GameState
-- Moving stone
updateRobot' gs cmd field fMap rPos newPos target newPos2 | 
           (cmd == CmdR || cmd == CmdL) && cellIsRock target && atPos fMap newPos2 == Empty =
               let newHeap = addCellsReq (gsUpdateList gs) [newPos2, rPos, newPos]
               in do newMap <- mapUpdate fMap [(rPos, Empty), (newPos, Robot), (newPos2, target)]
                     return gs{ gsUpdateList = newHeap,
                             gsCurScore = gsCurScore gs - 1,
                             gsField = field {
                                       gfRobotPos = newPos,
                                       gfMap = newMap
                                      }}
-- Entering open lift
updateRobot' gs cmd field fMap rPos newPos target newPos2 | 
            target == OpenLift = do
               newMap <- mapUpdate fMap [(rPos, Empty), (newPos, Robot)]
               return 
                  gs {
                      gsCurScore = gsCurScore gs + (50 * gfCurLambdas field),
                      gsStatus = Win,
                      gsField = field {
                                       gfMap = newMap,
                                       gfRobotPos = newPos
                                      }}
-- Moving to the next position
updateRobot' gs cmd field fMap rPos newPos target newPos2 |
             target == Empty || target == Earth || target == Lambda || target == Razor =
              let lambdaCoef = if target == Lambda then 1 else 0
                  razorCoef = if target == Razor then 1 else 0
                  newHeap = addCellsReq (gsUpdateList gs) [rPos, newPos]
              in do newMap <- mapUpdate fMap [(rPos, Empty), (newPos, Robot)]
                    return gs {
                               gsCurScore = gsCurScore gs - 1 + (25 * lambdaCoef),
                               gsUpdateList = newHeap,
                               gsField = field {
                                     gfMap = newMap,
                                     gfRobotPos = newPos,
                                     gfCurLambdas = gfCurLambdas field + lambdaCoef,
                                     gfCurRazors = gfCurRazors field + razorCoef
                                    }}
-- Entering trampoline
updateRobot' gs cmd field fMap rPos newPos (Trampoline c) newPos2 =
    let trPosMap = gfgTrampolinePos $ gfGlobals field
        trTargetMap = gfgTrampolineTarget $ gfGlobals field
        trTarget = head $ fromJust $ M.lookup c trTargetMap
        toPos = fromJust $ M.lookup trTarget trPosMap
        deadTrs = fromJust $ M.lookup trTarget trTargetMap
        deadPosLst = map (\t -> fromJust $ M.lookup t trPosMap) deadTrs
     
        newHeap = addCellsReq (gsUpdateList gs) $ [rPos, toPos] ++ deadPosLst
    in do
        newMap <- mapUpdate fMap $ [(rPos, Empty), (toPos, Robot)] ++ map (\ p -> (p, Empty)) deadPosLst
        return 
          gs {
              gsCurScore = gsCurScore gs - 1,
              gsUpdateList = newHeap,
              gsField = field {
                            gfMap = newMap,
                            gfRobotPos = toPos
                           }}
-- Other command is equivalent to wait
updateRobot' gs _ _ _ _ _ _ _ = updateRobot gs CmdW

getRockTarget :: FieldMap -> Pos -> Maybe Pos
getRockTarget oldMap pos | atPos oldMap (pos + (Pos 0 (-1))) == Empty        = Just (pos + (Pos 0 (-1)))  

getRockTarget oldMap pos | cellIsRock (atPos oldMap (pos + (Pos 0 (-1)))) &&
                           atPos oldMap (pos + (Pos 1 0)) == Empty &&  
                           atPos oldMap (pos + (Pos 1 (-1))) == Empty        = Just (pos + (Pos 1 (-1)))  

getRockTarget oldMap pos | cellIsRock (atPos oldMap (pos + (Pos 0 (-1)))) &&
                           atPos oldMap (pos + (Pos (-1) 0)) == Empty &&  
                           atPos oldMap (pos + (Pos (-1) (-1))) == Empty     = Just (pos + (Pos (-1) (-1)))  

getRockTarget oldMap pos | atPos oldMap (pos + (Pos 0 (-1))) == Lambda &&
                           atPos oldMap (pos + (Pos 1 0)) == Empty &&  
                           atPos oldMap (pos + (Pos 1 (-1))) == Empty        = Just (pos + (Pos 1 (-1)))

getRockTarget _ _                                                            = Nothing

updateCell :: Monad m => GameState -> FieldMap -> PosHeap -> Pos -> Int -> [Pos] -> GameM m (FieldMap, PosHeap, Int, [Pos])
updateCell gs newMap newHeap pos numBeards beardList = updateCell' gs pos (gfMap $ gsField $ gs)
                                                       newMap newHeap 
                                                       numBeards beardList (atPos (gfMap $ gsField $ gs) pos)

updateCell' :: Monad m => GameState -> Pos -> FieldMap -> FieldMap -> PosHeap -> Int -> [Pos] -> Cell -> GameM m (FieldMap, PosHeap, Int, [Pos])
updateCell' gs pos oldMap newMap newHeap numBeards beardList cell | cellIsRock cell = 
                                      case getRockTarget oldMap pos of
                                        Just newPos -> 
                                            let
                                                newHeap1 = addCellsPos (addCellsReq newHeap [pos]) [newPos]
                                            in do
                                                newMap1 <- mapUpdate newMap [(pos, Empty), 
                                                                             (newPos, cellMakeFalling cell (atPos oldMap $ newPos + (Pos 0 (-1))))]
                                                return (newMap1, newHeap1, numBeards, beardList)
                                        Nothing -> do
                                          newMap1 <- mapUpdate newMap [(pos, cellEndFall cell)]
                                          return (newMap1, newHeap, numBeards, beardList)
updateCell' gs pos oldMap newMap newHeap numBeards beardList ClosedLift = 
                                            let noMoreLambdas = gfCurLambdas (gsField gs) == (gfgTotalLambdas (gfGlobals (gsField gs)))
                                            in if noMoreLambdas
                                               then do
                                                     newMap1 <- mapUpdate newMap [(pos, OpenLift)]
                                                     return (newMap1, newHeap, numBeards, beardList)
                                                else return (newMap, addCellsPos newHeap [pos], numBeards, beardList)
updateCell' gs pos oldMap newMap newHeap numBeards beardList Beard | gfCurGrowth (gsField gs) == 1 = 
                                             let newBeards = filter (\ p -> atPos oldMap p == Empty && atPos newMap p /= Beard) 
                                                                    [Pos x y | x <- [posX pos - 1 .. posX pos + 1],
                                                                               y <- [posY pos - 1 .. posY pos + 1]]
                                             in do
                                                 newMap1 <- mapUpdate newMap $ map (\ p -> (p, Beard)) newBeards
                                                 return (newMap1, newHeap, numBeards + length newBeards, newBeards ++ beardList)
updateCell' gs pos oldMap newMap newHeap numBeards beardList _ = return (newMap, newHeap, numBeards, beardList)

updateMap :: Monad m => GameState -> GameM m (FieldMap, PosHeap, Int, [Pos])
updateMap gs = let needBeardUpdate = gfCurGrowth (gsField gs) == 1
                   newHeap = H.empty
                   newMap = gfMap $ gsField gs
                   newBeardNum = gfNumBeards $ gsField gs
                   newBeardList = if needBeardUpdate
                                  then []
                                  else gfPossibleBeards $ gsField gs
                   updOne :: Monad m => (Pos, (FieldMap, PosHeap, Int, [Pos])) -> Pos -> GameM m (Pos, (FieldMap, PosHeap, Int, [Pos]))
                   updOne (lastPos, lastRes@(m, h, nb, bl)) pos = if pos == lastPos
                                                                    then return (pos, lastRes)
                                                                    else do res <- updateCell gs m h pos nb bl
                                                                            return (pos, res)
               in do (_, res) <- F.foldlM updOne ((Pos (-1) (-1)), (newMap, newHeap, newBeardNum, newBeardList)) (gsUpdateList gs)
                     return res

haveLost :: FieldMap -> Pos -> Bool
haveLost map pos = let above = atPos map (pos + Pos 0 1)
                   in above == FallingRock || above == FallingHOR 

isUnderWater :: Int -> Pos -> Bool
isUnderWater level pos = posY pos <= level

updateState :: Monad m => GameState -> Command -> GameM m GameState
updateState gs cmd = do
  gs' <- updateRobot gs cmd
  (newMap, newHeap, newNumBeard, newBeardList) <- updateMap gs'
  let newGrowth = if gfCurGrowth (gsField gs') == 1
                     then (gfgMaxGrowth (gfGlobals (gsField gs')))
                     else if gfCurGrowth (gsField gs') > 0
                            then gfCurGrowth (gsField gs') - 1
                            else 0
  let needBeardUpdateAfter = newGrowth == 1
  let field = gsField gs'
  let newWaterLevel = if gfFloodingCounter field == 1
                        then gfWaterLevel field + 1
                        else gfWaterLevel field
  let newWaterProof = if isUnderWater newWaterLevel (gfRobotPos field)
                        then gfCurWaterProof field - 1
                        else gfgWaterProof $ gfGlobals field
  return gs' {
              gsStatus = if haveLost newMap (gfRobotPos field) || newWaterProof < 0
                           then Lost
                           else gsStatus gs' ,
              gsUpdateList = if needBeardUpdateAfter
                               then addCellsPos newHeap newBeardList
                               else newHeap ,
              gsPath = cmd : gsPath gs' ,
              gsField = field {
                               gfMap = newMap,
                               gfWaterLevel = newWaterLevel,
                               gfCurWaterProof = newWaterProof,
                               gfFloodingCounter = if gfFloodingCounter field <= 1
                                                     then gfgFlooding $ gfGlobals field
                                                     else gfFloodingCounter field - 1 ,
                               gfCurGrowth = newGrowth,
                               gfNumBeards = newNumBeard,
                               gfPossibleBeards = newBeardList
                              }
             }

initState :: Monad m => GameState -> GameM m GameState
initState gs = do
  newMap <- initTreeCache $ gfMap $ gsField gs
  return gs {
             gsField = (gsField gs) {
                                     gfMap = newMap
                                    }
            }