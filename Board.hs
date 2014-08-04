--{-# LANGUAGE TemplateHaskell #-}

module Board (
      Board ( Board )
    , state
    , isOccupied
    , getActiveAtPos
    , updateBoard
    , makeBoard
    ) where

import Haste

import Lens.Family2
import Lens.Family2.Stock
import Lens.Family2.Unchecked
--import Control.Lens
--import System.Random
import Data.List

import Pieces
import Team
import ActivePiece
import BoardDimensions
import BoardState
import Utils

type SpawnPoint = Position

data Board = Board { 
      _state :: BoardState
    , _activePieces :: [ActivePiece]
    , _spawnPoints :: [SpawnPoint]
    , _seed :: Seed
    }
--makeLenses ''Board

state :: Lens Board Board BoardState BoardState
state = lens _state (\s x -> s { _state = x })

activePieces :: Lens Board Board [ActivePiece] [ActivePiece]
activePieces = lens _activePieces (\s x -> s { _activePieces = x })

spawnPoints :: Lens Board Board [SpawnPoint] [SpawnPoint]
spawnPoints = lens _spawnPoints (\s x -> s { _spawnPoints = x })

seed :: Lens Board Board Seed Seed
seed = lens _seed (\s x -> s { _seed = x })

-- |getActiveAtPos finds the first ActivePiece at a position
getActiveAtPos :: Board -> Int -> Int -> Maybe ActivePiece
getActiveAtPos board x y = find (elem (x, y) . activePos) (board^.activePieces)

-- |emptyState returns a state with Empty everywhere
emptyState :: BoardState
emptyState = replicate boardWidth $ replicate boardHeight Empty

-- |makeBoard takes a set of spawn points and a seed and makes a board from it
makeBoard :: [SpawnPoint] -> Seed -> Board
makeBoard spawnPoints seed = Board emptyState aps spawnPoints newSeed
    where
        (aps, newSeed) = foldl (\(aps, s) f -> ((fst $ f s):aps, snd $ f s)) ([], seed) $ map spawnActivePieceAtPos spawnPoints

-- |spawnActivePieceAtPos takes a position and a seed and returns ActivePiece and the new seed
spawnActivePieceAtPos :: SpawnPoint -> Seed -> (ActivePiece, Seed)
spawnActivePieceAtPos spawnPoint seed = (ActivePiece spawnPoint pieceType First, newSeed)
    where
        (pieceType, newSeed) = randomPieceType seed

-- |updateBoard makes all pieces fall one step
updateBoard :: Board -> Board
updateBoard board = foldl f (board & activePieces .~ []) $ zip (board^.activePieces) (board^.spawnPoints)
    where
        -- f :: Board -> (activePiece :: ActivePiece, spawnPoint :: Position)
        f board (ap, sp)
            | respawn == False = board & activePieces %~ (newPiece:)
            | otherwise        = board &
                  (activePieces %~ (:) (newPiece &
                        (pos .~ sp) -- Move piece to spawn
                      . (pieceType .~ fst (randomPieceType $ board^.seed)) -- New type
                      . (orientation .~ First) -- Reset orientation
                  ))
                . (state %~ addPieceToState ap)
                . (seed %~ snd . randomPieceType)
                where
                    (newPiece, respawn) = updatePiece (board^.state) ap

-- Get positions of tiles of ActivePiece
activePos :: ActivePiece -> [Position]
activePos piece = map (addPos $ piece^.pos) $ shape (piece^.pieceType) (piece^.orientation)

addPieceToState :: ActivePiece -> BoardState -> BoardState
addPieceToState piece state = foldr addPosToState state $ activePos piece

addPosToState :: Position -> BoardState -> BoardState
--addPosToState (x, y) state = state & element x . element y .~ Occupied
addPosToState (x, y) state = replace2 x y Occupied state
