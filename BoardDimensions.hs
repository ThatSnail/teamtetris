module BoardDimensions (
      boardWidth
    , boardHeight
    , spawns
    ) where

boardWidth :: Int
boardWidth = 10

boardHeight :: Int
boardHeight = 22

type Position = (Int, Int)
spawns :: Int -> [Position]
spawns n = zip (map (* (boardWidth `div` (n + 1))) [1..n]) $ repeat (boardHeight - 1)
