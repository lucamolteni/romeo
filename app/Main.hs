module Main where

import Lib
import Control.Monad


main :: IO ()
main = do putStrLn "Insert Player Name:"
          playerName <- getLine
          let newPlayer = Player playerName 0 0
          forever $ echo newPlayer


echo :: Player -> IO ()
echo playerName = do putStrLn $ name playerName ++ ", insert a direction:"
                     input <- getLine
                     let direction = parseDirection input
                     let newPos = moveToNewPos playerName direction
                     print newPos
                     return ()


moveDirF = flip moveDir

moveToNewPos :: Player -> [Direction] -> [Player]
moveToNewPos player dir = dir >>= moveDirF player

parseDirection :: String -> [Direction]
parseDirection "N" = [N]
parseDirection "S" = [S]
parseDirection "E" = [E]
parseDirection "W" = [W]
parseDirection _ = []
