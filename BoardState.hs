module BoardState (
      TileState ( Empty, Occupied )
    , BoardState
    , isOccupied
    ) where

import Data.List.Split
import Binary

data TileState = Empty | Occupied deriving Eq
type BoardState = [[TileState]]

-- |isOccupied checks to see if a position has an inactive tile at a position
isOccupied :: BoardState -> Int -> Int -> Bool
isOccupied boardState x y = (boardState !! x !! y) == Occupied

encodeBoardState :: BoardState -> String
encodeBoardState boardState = map toAscii $ chunksOf 8 $ map bin $ concat boardState
    where
        bin Empty    = 0
        bin Occupied = 1

decodeBoardState :: String -> Int -> Int -> BoardState
decodeBoardState encoded boardWidth boardHeight = chunksOf boardWidth $ take (boardWidth * boardHeight) $ map unbin $ concat $ map toBits encoded
    where
        unbin 0 = Empty
        unbin 1 = Occupied
