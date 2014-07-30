module ActivePiece (
      ActivePiece ( ActivePiece )
    , updatePiece
    ) where

import Player
import Pieces
import Utils
import BoardDimensions

data ActivePiece = ActivePiece {
      player :: Player
    , pos :: Position
    , pieceType :: PieceType
    , orientation :: Orientation
    }

-- Returns new piece and whether or not to respawn a piece
updatePiece :: ActivePiece -> (ActivePiece, Bool)
updatePiece piece@(ActivePiece { pos = (px, py), pieceType = pieceType', orientation = orientation' })
    | validPos pieceType' orientation' npos = (piece { pos = npos }, False)
    | otherwise                = (piece, True)
        where
            npos = (px, py - 1)

validPos :: PieceType -> Orientation -> Position -> Bool
validPos pieceType orientation (px, py) = foldr1 (&&) $ map (validPos' . (\(x, y) -> (x + px, y + py)))  $ shape pieceType orientation
    where
        validPos' (x, y) = 0 <= x          &&
                           x < boardWidth  &&
                           0 <= y          &&
                           y < boardHeight
