module Main where

import Lib
import Control.Monad


main :: IO ()
main = forever echo


echo = do putStrLn "Insert a direction:"
          direction <- getLine
          print direction
          return ()
