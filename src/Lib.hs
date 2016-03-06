module Lib where

moveDirF :: Direction -> Player -> Player
moveDirF N = up
moveDirF E = right
moveDirF S = down
moveDirF W = left

move :: Player -> Direction -> Player
move = flip moveDirF

parseDirection :: String -> Maybe Direction
parseDirection "N" = Just N
parseDirection "S" = Just S
parseDirection "E" = Just E
parseDirection "W" = Just W
parseDirection _ = Nothing

boardSize :: Int
boardSize = 10

data Direction = N | S | E | W deriving Show

data Player = Player {
    name :: String
  , x    :: Int
  , y    :: Int
}

instance Show Player where
  show (Player name x y) = name ++ " x:" ++ show x ++ " y:" ++ show y

overflow :: Int -> Bool
overflow newPos = newPos > boardSize || newPos < 0

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
