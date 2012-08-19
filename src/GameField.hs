module GameField
    where

import QuadTree
import Pos
import qualified Data.Map as M
import Data.List

data Cell = Robot
          | Beard
          | Razor
          | HigherOrderRock
          | FallingHOR
          | Wall
          | Rock
          | Lambda
          | ClosedLift
          | OpenLift
          | Earth
          | Empty
          | FallingRock
          | Trampoline !Char
          | Target !Char
            deriving (Eq, Ord)

instance Show Cell where
    show Robot = "R"
    show Beard = "W"
    show Razor = "!"
    show HigherOrderRock = "@"
    show FallingHOR = "@"
    show Wall = "#"
    show Rock = "*"
    show Lambda = "\\"
    show ClosedLift = "L"
    show OpenLift = "O"
    show Earth = "."
    show Empty = " "
    show FallingRock = "*"
    show (Trampoline c) = [c]
    show (Target c) = [c]

charToCell :: Char -> Cell
charToCell 'R' = Robot
charToCell 'W' = Beard
charToCell '!' = Razor
charToCell '@' = HigherOrderRock
charToCell '#' = Wall
charToCell '*' = Rock
charToCell '\\' = Lambda
charToCell 'L' = ClosedLift
charToCell 'O' = OpenLift
charToCell '.' = Earth
charToCell ' ' = Empty
charToCell ch | ch >= 'A' && ch <= 'I' = Trampoline ch
              | ch >= '0' && ch <= '9' = Target ch


cellIsRock :: Cell -> Bool
cellIsRock Rock = True
cellIsRock FallingRock = True
cellIsRock HigherOrderRock = True
cellIsRock FallingHOR = True
cellIsRock _ = False

cellEndFall :: Cell -> Cell
cellEndFall Rock = Rock
cellEndFall FallingRock = Rock
cellEndFall HigherOrderRock = HigherOrderRock
cellEndFall FallingHOR = Lambda

cellMakeFalling :: Cell -> Cell -> Cell
cellMakeFalling val below = let checkBelow Robot newFalling _ = newFalling
                                checkBelow Empty newFalling _ = newFalling
                                checkBelow _ _ newStill = newStill
                            in case val of
                                 Rock -> checkBelow below FallingRock Rock
                                 FallingRock -> checkBelow below FallingRock Rock
                                 HigherOrderRock -> checkBelow below FallingHOR HigherOrderRock
                                 FallingHOR -> checkBelow below FallingHOR HigherOrderRock

type FieldMap = CachedTree Cell

atPos :: FieldMap -> Pos -> Cell
atPos m pos = atPosWithDef m Empty pos 

showField :: FieldMap -> String
showField m = let pre = treeShow m
                  noempty = filter (/= "") pre
                  noHBorders = drop 2 $ take (length noempty - 2) noempty
                  noVBordes = map (\s -> drop 2 $ take (length s - 2) s) noHBorders
              in unlines $ reverse noVBordes

data GameFieldGlobals = GameFieldGlobals { gfgFlooding :: !Int, gfgWaterProof :: !Int, 
                                           gfgTrampolineTarget :: !(M.Map Char [Char]),
                                           gfgTrampolinePos :: !(M.Map Char Pos),
                                           gfgMaxGrowth :: !Int, gfgLiftPos :: !Pos}

data GameField = GameField { gfMap :: !FieldMap, gfRobotPos :: !Pos,
                             gfCurLambdas :: !Int, gfWaterLevel :: !Int,
                             gfFloodingCounter :: !Int, gfCurWaterProof :: !Int,
                             gfNumBeards :: !Int, gfPossibleBeards :: ![Pos], 
                             gfCurGrowth :: !Int, gfCurRazors :: !Int,
                             gfGlobals :: !GameFieldGlobals }

processMap :: FieldMap -> (Cell -> Pos -> a -> a) -> Int -> Int -> a -> a
processMap m func w h init = foldr (\ (c, pos) st -> func c pos st) init
                                   [ (atPos m (Pos x y), (Pos x y)) | x <- [0..w-1], y <- [0..h-1]]

data ReadFieldMapState = ReadFieldMapState { msNumLambdas :: !Int, msRobotPos :: !Pos, msNumBeards :: !Int,
                                             msBeards :: ![Pos],
                                             msTrampolines :: !(M.Map Char Pos), msLiftPos :: !Pos}

initReadState :: ReadFieldMapState
initReadState = ReadFieldMapState 0 (Pos 0 0) 0 [] M.empty (Pos 0 0)

updReadState :: Cell -> Pos -> ReadFieldMapState -> ReadFieldMapState
updReadState Lambda          _   st@ReadFieldMapState{ msNumLambdas = nl } = st{msNumLambdas = nl + 1}
updReadState HigherOrderRock _   st@ReadFieldMapState{ msNumLambdas = nl } = st{msNumLambdas = nl + 1}
updReadState ClosedLift      pos st                                        = st{msLiftPos = pos}
updReadState Robot           pos st                                        = st{msRobotPos = pos}
updReadState (Trampoline c)  pos st@ReadFieldMapState{ msTrampolines = tr} = st{msTrampolines = M.insert c pos tr}
updReadState (Target c)      pos st@ReadFieldMapState{ msTrampolines = tr} = st{msTrampolines = M.insert c pos tr}
updReadState Beard           pos st@ReadFieldMapState{ msNumBeards = nb,
                                                       msBeards = bl}      = st{msNumBeards = nb + 1, msBeards = pos : bl}
updReadState c _ st = st 

type Metadata = M.Map String [[String]]

readMetadata :: Metadata -> String -> a -> ([[String]] -> a) -> a
readMetadata map key defVal func = case M.lookup key map of
                                     Nothing -> defVal
                                     Just v -> func v

fillMetadata :: [String] -> Metadata
fillMetadata strs = foldr addKey M.empty $ filter (/= "") strs
    where addKey str m = 
              let ws = words str
                  key = head ws
                  val = tail ws
              in M.insertWith (++) key [val] m

makeTrampolineTargetsMap :: [[String]] -> M.Map Char [Char]
makeTrampolineTargetsMap strs = foldr addKey M.empty strs
    where addKey [[from], _, [to]] m = M.insertWith (++) to [from]
                                        (M.insert from [to] m)


readField :: [String] -> GameField
readField strs = let (rows'' , metadataStrs) = break (== "") strs
                     -- y is counted from the bottom
                     rows' = reverse rows''
                     -- adding walls
                     w' = maximum $ map length rows'
                     h' = length rows'
                     walls = replicate (w' + 4) '#'
                     rowsW = map (\ s -> "##" ++ s ++ "##") rows'
                     rows = [walls, walls] ++ rowsW ++ [walls, walls]
                     w = w' + 4
                     h = h' + 4
                     -- reading map
                     field = readTree (map (map charToCell) rows)
                     metadata = fillMetadata metadataStrs
                     fieldData = processMap field updReadState w h initReadState
                     globals = GameFieldGlobals {
                                  gfgFlooding = readMetadata metadata "Flooding" 0 (read . head . head),
                                  gfgWaterProof = readMetadata metadata "Waterproof" 10 (read . head . head),
                                  gfgTrampolineTarget = readMetadata metadata "Trampoline" M.empty makeTrampolineTargetsMap,
                                  gfgTrampolinePos = msTrampolines fieldData,
                                  gfgMaxGrowth = readMetadata metadata "Growth" 25 (read . head . head),
                                  gfgLiftPos = msLiftPos fieldData
                                }
                 in GameField {
                               gfMap = field,
                               gfRobotPos = msRobotPos fieldData,
                               gfCurLambdas = msNumLambdas fieldData,
                               gfWaterLevel = 1 + readMetadata metadata "Water" 0 (read . head . head),
                               gfFloodingCounter = gfgFlooding globals,
                               gfCurWaterProof = gfgWaterProof globals,
                               gfNumBeards = msNumBeards fieldData,
                               gfPossibleBeards = msBeards fieldData,
                               gfCurGrowth = gfgMaxGrowth globals,
                               gfCurRazors = readMetadata metadata "Razors" 0 (read . head . head),
                               gfGlobals = globals
                              }
