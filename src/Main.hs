module Main
    where

import ManualBot
import QuadTree
import System.Environment

main :: IO ()
main = do args <- getArgs
          let fname = args !! 0
          runNodeCacheM (runManual fname)