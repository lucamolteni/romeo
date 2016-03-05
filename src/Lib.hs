module Lib where

someFunc :: IO ()
someFunc = do let p1 = (right . right . down) player1
              let p2 = (down . down ) player2
              print p1
              print p2

move :: Player -> Player
move = right . right

boardSize :: Int
boardSize = 10

player1 = Player "player1" 0 0
player2 = Player "player2" 4 4


data Player = Player { name :: String
  ,  x :: Int
  , y :: Int
} deriving Show

right :: Player -> Player
right (Player name x y) = Player name (x + 1) y

left :: Player -> Player
left (Player name x y) = Player name (x - 1) y

up :: Player -> Player
up (Player name x y) = Player name x (y + 1)

down :: Player -> Player
down (Player name x y) = Player name x (y - 1)
