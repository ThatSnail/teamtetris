module Board (
      Board ( Board )
    , isOccupied
    , boardWidth
    , boardHeight
-- Temp
    , emptyBoard
    ) where

data TileState = Empty | Occupied deriving Eq
type State = [[TileState]]

data Board = Board { 
      state :: State
    }

boardWidth :: Int
boardWidth = 10

boardHeight :: Int
boardHeight = 22

isOccupied :: Board -> Int -> Int -> Bool
isOccupied board x y = ((state board) !! x !! y) == Occupied

emptyState :: State
emptyState = replicate boardHeight $ replicate boardWidth Empty

emptyBoard :: Board
emptyBoard = Board emptyState
