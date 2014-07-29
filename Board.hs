module Board (
      Board ( Board )
    , isOccupied
    ) where

data TileState = Empty | Occupied
type State = [[TileState]]

data Board = Board { 
      state :: State
    }

width :: Int
width = 10

height :: Int
height = 12

isOccupied :: Board -> Int -> Int -> TileState
isOccupied board x y = (state board) !! x !! y

emptyState :: State
emptyState = replicate height $ replicate width Empty

emptyBoard :: Board
emptyBoard = Board emptyState
