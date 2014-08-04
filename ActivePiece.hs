--{-# LANGUAGE TemplateHaskell #-}

module ActivePiece (
      ActivePiece ( ActivePiece, _pos, _pieceType, _orientation )
    , pos
    , pieceType
    , orientation
    , updatePiece
    ) where

import Lens.Family2
import Lens.Family2.Stock
import Lens.Family2.Unchecked
import Control.Applicative

import Player
import Pieces
import Utils
import BoardState
import BoardDimensions

data ActivePiece = ActivePiece {
      _pos :: Position
    , _pieceType :: PieceType
    , _orientation :: Orientation
    }

pos :: Lens ActivePiece ActivePiece Position Position
pos = lens _pos (\s x -> s { _pos = x })

pieceType :: Lens ActivePiece ActivePiece PieceType PieceType
pieceType = lens _pieceType (\s x -> s { _pieceType = x })

orientation :: Lens ActivePiece ActivePiece Orientation Orientation
orientation = lens _orientation (\s x -> s { _orientation = x })

--makeLenses ''ActivePiece

-- |updatePiece returns new piece and whether or not to respawn a piece
updatePiece :: BoardState -> ActivePiece -> (ActivePiece, Bool)
updatePiece boardState piece
    | canStillFall boardState (piece^.pieceType) (piece^.orientation) npos = (piece & pos .~ npos, False)
    | otherwise                                             = (piece, True)
        where
            npos = (piece^.pos) & _2 %~ (-)1

-- |canStillFall checks to see if the piece does not collide with any board elements
-- It does not check collisions with other ActivePieces
canStillFall :: BoardState -> PieceType -> Orientation -> Position -> Bool
canStillFall boardState pieceType orientation pos = all (((&&) . validPos' <*> notColliding) . addPos pos) $ shape pieceType orientation
    where
        validPos' (x, y) = 0 <= x          &&
                           x < boardWidth  &&
                           0 <= y          &&
                           y < boardHeight
        notColliding (x, y) = not $ isOccupied boardState x y
