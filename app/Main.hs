module Main where

import           Control.Monad
import           Data.Maybe
import           Lib


main :: IO ()
main = do putStrLn "Insert Player Name:"
          playerName <- getLine
          let newPlayer = Player playerName 0 0
          loop [newPlayer]

loop :: [Player] -> IO ()
loop initialState = do  let player = last initialState
                        putStrLn $ name player ++ ", insert a direction:"
                        input <- getLine
                        let direction = parseDirection input
                        let newPos = fmap (move player) direction
                        let allPos = initialState ++ maybeToList newPos
                        mapM_ print allPos
                        loop allPos


lastWithDefault :: [a] -> a -> a
lastWithDefault list def = if null list then def else last list
