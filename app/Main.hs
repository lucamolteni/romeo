module Main where

import Lib
import Control.Monad


main :: IO ()
main = do putStrLn "Insert Player Name:"
          playerName <- getLine
          let newPlayer = Player playerName 0 0
          forever $ echo newPlayer


echo :: Player -> IO ()
echo player = do putStrLn $ name player ++ ", insert a direction:"
                 input <- getLine
                 let direction = parseDirection input
                 let newPos = moveToNewPos player direction
                 print newPos
                 return ()
