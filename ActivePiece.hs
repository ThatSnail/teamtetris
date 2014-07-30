{-# LANGUAGE TemplateHaskell #-}

module ActivePiece (
      ActivePiece ( ActivePiece, _player, _pos, _pieceType, _orientation )
    , pos
    , pieceType
    , orientation
    , updatePiece
    ) where

import Control.Lens

import Player
import Pieces
import Utils
import BoardDimensions

data ActivePiece = ActivePiece {
      _player :: Player
    , _pos :: Position
    , _pieceType :: PieceType
    , _orientation :: Orientation
    }
makeLenses ''ActivePiece

-- Returns new piece and whether or not to respawn a piece
updatePiece :: ActivePiece -> (ActivePiece, Bool)
updatePiece piece
    | validPos (piece^.pieceType) (piece^.orientation) npos = (piece & pos .~ npos, False)
    | otherwise                                             = (piece, True)
        where
            npos = (piece^.pos) & _2 %~ ((-) 1)

validPos :: PieceType -> Orientation -> Position -> Bool
validPos pieceType orientation (px, py) = foldr1 (&&) $ map (validPos' . (\(x, y) -> (x + px, y + py))) $ shape pieceType orientation
    where
        validPos' (x, y) = 0 <= x          &&
                           x < boardWidth  &&
                           0 <= y          &&
                           y < boardHeight
