module Main where

import           Control.Monad
import           Data.Maybe
import           Lib


main :: IO ()
main = do player1 <- askForPlayerName
          loop [] player1

askForPlayerName :: IO Player
askForPlayerName = do putStrLn "Insert Player Name:"
                      playerName <- getLine
                      return $ Player playerName 0 0

loop :: [Player] -> Player -> IO ()
loop initialState player1 = do  newPlayer1 <- movePlayerLoop player1
                                let log = initialState ++ [newPlayer1]
                                mapM_ print log
                                loop log newPlayer1

movePlayerLoop :: Player -> IO Player
movePlayerLoop player = do putStrLn $ name player ++ ", insert a direction:"
                           input <- getLine
                           return $ movePlayer player input

movePlayer :: Player -> String -> Player
movePlayer p input = fromMaybe p newPlayer1
    where newPlayer1 = fmap (move p) (parseDirection input)


lastWithDefault :: [a] -> a -> a
lastWithDefault list def = if null list then def else last list
