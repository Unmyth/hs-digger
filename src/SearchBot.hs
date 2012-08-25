{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module SearchBot
    where

import QuadTree
import GameField
import GameState
import Pos
import Control.Monad.IO.Class
import System.IO
import AStar
import Data.Ord
import Data.Monoid
import ManualBot

type SearchM = GameM IO

instance Eq GameState where
    gs1 == gs2 = let field1 = gsField gs1
                     field2 = gsField gs2
                 in (treeIdx $ gfMap field1) == (treeIdx $ gfMap field2) &&
                      (gfWaterLevel field1) == (gfWaterLevel field2) &&
                      (gfFloodingCounter field1) == (gfFloodingCounter field2) &&
                      (gfCurWaterProof field1) == (gfCurWaterProof field2) &&
                      (gfCurGrowth field1) == (gfCurGrowth field2) &&
                      (gfCurRazors field1) == (gfCurRazors field2)

instance Ord GameState where
    gs1 `compare` gs2 = let field1 = gsField gs1
                            field2 = gsField gs2
                        in ((treeIdx $ gfMap field1) `compare` (treeIdx $ gfMap field2)) `mappend`
                               ((gfWaterLevel field1) `compare` (gfWaterLevel field2)) `mappend`
                               ((gfFloodingCounter field1) `compare` (gfFloodingCounter field2)) `mappend`
                               ((gfCurWaterProof field1) `compare` (gfCurWaterProof field2)) `mappend`
                               ((gfCurGrowth field1) `compare` (gfCurGrowth field2)) `mappend`
                               ((gfCurRazors field1) `compare` (gfCurRazors field2))

instance SearchState GameState where
    type SearchMonad GameState = SearchM
    nextStates st = mapM (updateState st) $ [CmdU, CmdL, CmdR, CmdD, CmdW {- CmdA, -}] ++
                                (if (gfNumBeards $ gsField st) == 0 then [] else [CmdS])
    estimate st = let field = gsField st
                      pathLen = length $ gsPath st
                      toGo = distance (gfRobotPos field) (gfgLiftPos $ gfGlobals field)
                      lambdaScore = 100 * gfgTotalLambdas (gfGlobals field)
                  in pathLen + toGo - lambdaScore
    finalState st = gsStatus st == Win
    deadState st = gsStatus st == Lost
    report st cnt = if cnt `mod` 1000 == 0
                      then do liftIO $ putStrLn $ "Iteration " ++ show cnt
                              printGS st
                      else return ()

runSearch :: String -> SearchM ()
runSearch fname = 
    do content <- liftIO $ readFile fname
       let field = readField $ lines content
       let gs' = makeGameState field
       gs <- initState gs'
       res <- aStar gs
       case res of
         Nothing -> liftIO $ putStrLn "Not found"
         Just gs1 -> printGS gs1
       return ()
       

