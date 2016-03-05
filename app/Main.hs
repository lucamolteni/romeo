module Main where

import           Control.Monad
import           Lib


main :: IO ()
main = do putStrLn "Insert Player Name:"
          playerName <- getLine
          let newPlayer = Player playerName 0 0
          loop [] newPlayer

loop :: [Player] -> Player -> IO ()
loop initialState player = do putStrLn $ name player ++ ", insert a direction:"
                              input <- getLine
                              let direction = parseDirection input
                              let newPos = initialState ++ moveToNewPos player direction
                              mapM_ print newPos
                              let newPlayer = lastWithDefault newPos player
                              loop newPos newPlayer


lastWithDefault :: [a] -> a -> a
lastWithDefault list def = if null list then def else last list
