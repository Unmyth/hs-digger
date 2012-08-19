module ManualBot
    where

import QuadTree
import GameField
import GameState
import Pos
import Control.Monad.IO.Class
import System.IO

type ManualM = GameM IO

printGS :: GameState -> ManualM ()
printGS gs = do
    liftIO $ putStrLn $ showField $ gfMap $ gsField gs
    liftIO $ putStrLn $ "Current score = " ++ show (gsCurScore gs)
    liftIO $ putStrLn $ "Path = " ++ concat (reverse $ map show $ gsPath gs)
    liftIO $ putStrLn $ "Status = " ++ show (gsStatus gs)
    liftIO $ putStrLn $ "Water level = " ++ show (gfWaterLevel (gsField gs))
    liftIO $ putStrLn $ "Robot is at " ++ show (gfRobotPos (gsField gs))

iter :: GameState -> ManualM ()
iter gs = do
  cmdChar <- liftIO $ hSetBuffering stdin NoBuffering >> getChar
  liftIO $ putStrLn ""
  let cmd = manualCharToCmd cmdChar
  gs' <- updateState gs cmd
  printGS gs'
  if gsStatus gs' == InProgress
      then iter gs'
      else return ()

runManual :: String -> ManualM ()
runManual fname = 
    do content <- liftIO $ readFile fname
       let field = readField $ lines content
       let gs' = makeGameState field
       gs <- initState gs'
       printGS gs
       iter gs
