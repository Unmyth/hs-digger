module Main
    where

import ManualBot
import QuadTree
import System.Environment
import SearchBot

main :: IO ()
main = do args <- getArgs
          let fname = args !! 0
          --runNodeCacheM (runManual fname)
          runNodeCacheM (runSearch fname)