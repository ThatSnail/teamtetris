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

import Player
import Pieces
import Utils
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

-- Returns new piece and whether or not to respawn a piece
updatePiece :: ActivePiece -> (ActivePiece, Bool)
updatePiece piece
    | validPos (piece^.pieceType) (piece^.orientation) npos = (piece & pos .~ npos, False)
    | otherwise                                             = (piece, True)
        where
            npos = (piece^.pos) & _2 %~ (\y -> y - 1)

validPos :: PieceType -> Orientation -> Position -> Bool
validPos pieceType orientation (px, py) = foldr1 (&&) $ map (validPos' . (\(x, y) -> (x + px, y + py))) $ shape pieceType orientation
    where
        validPos' (x, y) = 0 <= x          &&
                           x < boardWidth  &&
                           0 <= y          &&
                           y < boardHeight
