module Main where

import           Control.Monad
import           Data.Maybe
import           Lib


main :: IO ()
main = do putStrLn "Insert Player Name:"
          playerName <- getLine
          let player1 = Player playerName 0 0
          loop [] player1

loop :: [Player] -> Player -> IO ()
loop initialState player1 = do  putStrLn $ name player1 ++ ", insert a direction:"
                                input <- getLine
                                let direction = parseDirection input
                                let maybeNewPos =  fmap (move player1) direction
                                let loggedEvent = maybeToList maybeNewPos
                                let log = initialState ++ loggedEvent
                                mapM_ print log
                                loop log (lastWithDefault loggedEvent player1)

lastWithDefault :: [a] -> a -> a
lastWithDefault list def = if null list then def else last list
