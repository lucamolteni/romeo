module Main where

import           Control.Monad
import           Data.Maybe
import           Lib


main :: IO ()
main = do player1 <- askForPlayerName
          player2 <- askForPlayerName
          loop [] player1 player2

askForPlayerName :: IO Player
askForPlayerName = do putStrLn "Insert Player Name:"
                      playerName <- getLine
                      return $ Player playerName 0 0

loop :: [Player] -> Player -> Player -> IO ()
loop initialState player1 player2 = do  newPlayer1 <- movePlayerLoop player1
                                        let log1 = initialState ++ [newPlayer1]
                                        mapM_ print log1
                                        newPlayer2 <- movePlayerLoop player2
                                        let log2 = log1 ++ [newPlayer2]
                                        mapM_ print log2
                                        loop log2 newPlayer1 newPlayer2

movePlayerLoop :: Player -> IO Player
movePlayerLoop player = do putStrLn $ name player ++ ", insert a direction:"
                           input <- getLine
                           return $ movePlayer player input

movePlayer :: Player -> String -> Player
movePlayer p input = fromMaybe p newPlayer1
    where newPlayer1 = fmap (move p) (parseDirection input)
