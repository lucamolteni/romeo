module Lib where

someFunc :: IO ()
someFunc = do let p1 = (right . right . down) player1
              let p2 = (down . down ) player2
              print p1
              print p2

move :: (Player -> Player) -> Player -> [Player]
move moveFunc player = [player, newPos]
    where newPos = moveFunc player

moveDir :: Direction -> Player -> [Player]
moveDir N =  move up
moveDir E =  move right
moveDir S =  move down
moveDir W =  move left


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
