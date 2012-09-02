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
import Data.IORef
import Control.Monad.Reader
import System.Posix.Signals

type SearchM = GameM IO

data SGameState = SGS { sgsState :: !GameState, sgsEstimation :: !Int }

estimateGS :: GameState -> Int
estimateGS st = let field = gsField st
                    pathLen = length $ gsPath st
                    toGo = distance (gfRobotPos field) (gfgLiftPos $ gfGlobals field)
                in pathLen + toGo

updateSGS :: SGameState -> Command -> SearchM SGameState
updateSGS (SGS st _) cmd = do
  newST <- updateState st cmd
  let est = estimateGS newST
  return $ SGS newST est

instance Eq SGameState where
    SGS{ sgsState = gs1 } == SGS { sgsState = gs2 }
               = let field1 = gsField gs1
                     field2 = gsField gs2
                 in (treeIdx $ gfMap field1) == (treeIdx $ gfMap field2) &&
                      (gfWaterLevel field1) == (gfWaterLevel field2) &&
                      (gfFloodingCounter field1) == (gfFloodingCounter field2) &&
                      (gfCurWaterProof field1) == (gfCurWaterProof field2) &&
                      (gfCurGrowth field1) == (gfCurGrowth field2) &&
                      (gfCurRazors field1) == (gfCurRazors field2)

instance Ord SGameState where
    SGS{ sgsState = gs1 } `compare` SGS{ sgsState = gs2 } 
                      = let field1 = gsField gs1
                            field2 = gsField gs2
                        in ((treeIdx $ gfMap field1) `compare` (treeIdx $ gfMap field2)) `mappend`
                               ((gfWaterLevel field1) `compare` (gfWaterLevel field2)) `mappend`
                               ((gfFloodingCounter field1) `compare` (gfFloodingCounter field2)) `mappend`
                               ((gfCurWaterProof field1) `compare` (gfCurWaterProof field2)) `mappend`
                               ((gfCurGrowth field1) `compare` (gfCurGrowth field2)) `mappend`
                               ((gfCurRazors field1) `compare` (gfCurRazors field2))

instance SearchState SGameState where
    type SearchMonad SGameState = ReaderT (IORef Bool) SearchM
    nextStates sgs = mapM (lift . updateSGS sgs) $ [CmdU, CmdL, CmdR, CmdD, CmdW, CmdA ] ++
                                (if (gfNumBeards $ gsField $ sgsState sgs) == 0 then [] else [CmdS])
    estimate sgs = sgsEstimation sgs
    finalState SGS{ sgsState = st } = gsStatus st == Win
    deadState SGS{ sgsState = st } = gsStatus st == Lost
    report SGS{ sgsState = st } cnt = if cnt `mod` 1000 == 0
                                        then do liftIO $ putStrLn $ "Iteration " ++ show cnt
                                                lift $ printGS st
                                        else return ()
    curPenalty SGS { sgsState = st} = (- gsCurScore st)
    shouldQuit _ = do
      ref <- ask
      val <- liftIO $ readIORef ref 
      return val

runSearch :: String -> SearchM ()
runSearch fname = 
    do content <- liftIO $ readFile fname
       let field = readField $ lines content
       let gs' = makeGameState field
       gs <- initState gs'
       quitFlag <- liftIO $ newIORef False
       liftIO $ installHandler sigINT (Catch (writeIORef quitFlag True)) Nothing
       res <- runReaderT (aStar (SGS gs 0)) quitFlag
       case res of
         Nothing -> liftIO $ putStrLn "Not found"
         Just sgs1 -> printGS $ sgsState sgs1
       return ()
       

