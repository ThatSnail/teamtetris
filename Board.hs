{-# LANGUAGE TemplateHaskell #-}

module Board (
      Board ( Board )
    , isOccupied
    , updateBoard
    ) where

import Control.Lens
import System.Random

import Pieces
import Team
import ActivePiece
import BoardDimensions
import Utils

data TileState = Empty | Occupied deriving Eq
type State = [[TileState]]

data Board = Board { 
      _state :: State
    , _team :: Team
    , _activePieces :: [ActivePiece]
    , _spawnPoints :: [Position]
    , _seed :: StdGen
    }

makeLenses ''Board

isOccupied :: Board -> Int -> Int -> Bool
isOccupied board x y = ((_state board) !! x !! y) == Occupied

emptyState :: State
emptyState = replicate boardHeight $ replicate boardWidth Empty

updateBoard :: Board -> Board
updateBoard board = foldl f (board & activePieces .~ []) $ zip (board^.activePieces) (board^.spawnPoints)
    where
        -- f :: Board -> (activePiece :: ActivePiece, spawnPoint :: Position)
        f board (ap, sp)
            | snd nap == False = board & activePieces %~ ((fst nap):)
            | otherwise        = board & (activePieces %~ (((fst nap) & (pos .~ sp) . (pieceType .~ fst (randomPieceType (board^.seed))) . (orientation .~ First)):)) . (state %~ addPieceToState ap) . (seed %~ snd . randomPieceType)
                where
                    nap = updatePiece ap

addPieceToState :: ActivePiece -> State -> State
addPieceToState piece state = foldr addPosToState state $ map (\(x, y) -> (x + piece^.pos._1, y + piece^.pos._2)) $ shape (piece^.pieceType) (piece^.orientation)

addPosToState :: Position -> State -> State
addPosToState (x, y) state = state & element x . element y .~ Occupied
