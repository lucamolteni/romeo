module Lib where

move :: (Player -> Player) -> Player -> [Player]
move moveFunc player = [player, newPos]
    where newPos = moveFunc player

moveDirF :: Direction -> Player -> [Player]
moveDirF N =  move up
moveDirF E =  move right
moveDirF S =  move down
moveDirF W =  move left

moveDir :: Player -> Direction -> [Player]
moveDir = flip moveDirF

moveToNewPos :: Player -> [Direction] -> [Player]
moveToNewPos player dir = dir >>= moveDir player

parseDirection :: String -> [Direction]
parseDirection "N" = [N]
parseDirection "S" = [S]
parseDirection "E" = [E]
parseDirection "W" = [W]
parseDirection _ = []


boardSize :: Int
boardSize = 10

player1 = Player "player1" 0 0
player2 = Player "player2" 4 4

data Direction = N | S | E | W deriving Show

data Player = Player { name :: String
  ,  x :: Int
  , y :: Int
} deriving Show

overflow :: Int -> Bool
overflow newPos = newPos > boardSize || newPos <= 0

right :: Player -> Player
right (Player name x y) = let newXPos = x + 1 in
                          if overflow newXPos
                            then Player name x y
                            else Player name newXPos y

left :: Player -> Player
left (Player name x y) = let newXPos = x - 1 in
                          if overflow newXPos
                            then Player name x y
                            else Player name newXPos y

up :: Player -> Player
up (Player name x y) = let newYPos = y + 1 in
                          if overflow newYPos
                            then Player name x y
                            else Player name x newYPos

down :: Player -> Player
down (Player name x y) = let newYPos = y - 1 in
                          if overflow newYPos
                            then Player name x y
                            else Player name x newYPos
