--{-# LANGUAGE TemplateHaskell #-}

module ActivePiece (
      ActivePiece ( ActivePiece, _pos, _pieceType, _orientation )
    , pos
    , pieceType
    , orientation
    , updatePiece
    , activePos
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
updatePiece :: ActivePiece -> BoardState -> (ActivePiece, Bool)
updatePiece piece boardState
    | isFreeFromBoard (piece & pos .~ npos) boardState = (piece & pos .~ npos, False)
    | otherwise                                             = (piece, True)
        where
            npos = (piece^.pos) & _2 %~ subtract 1

move :: ActivePiece -> Position -> BoardState -> [ActivePiece] -> ActivePiece
move piece dpos boardState otherPieces
    | isFree newPiece boardState otherPieces = newPiece
    | otherwise                              = piece
    where
        newPiece = piece & pos %~ addPos dpos

isFreeFromPiece :: ActivePiece -> ActivePiece -> Bool
isFreeFromPiece piece1 piece2 = all (uncurry (/=)) $ liftA2 (,) (activePos piece1) (activePos piece2)

-- |isFree checks to see if the piece does not collide with anything: board, wall, or other ActivePieces
isFree :: ActivePiece -> BoardState -> [ActivePiece] -> Bool
isFree piece boardState otherPieces = (isFreeFromBoard piece boardState) && (foldr1 (&&) $ map (isFreeFromPiece piece) otherPieces)

-- |isFreeFromBoard checks to see if the piece does not collide with any board elements
-- It does not check collisions with other ActivePieces
isFreeFromBoard :: ActivePiece -> BoardState -> Bool
isFreeFromBoard piece boardState = all (((&&) . validPos' <*> notColliding) . addPos (piece^.pos)) $ shape (piece^.pieceType) (piece^.orientation)
    where
        validPos' (x, y) = 0 <= x          &&
                           x < boardWidth  &&
                           0 <= y          &&
                           y < boardHeight
        notColliding (x, y) = not $ isOccupied boardState x y

-- Get positions of tiles of ActivePiece
activePos :: ActivePiece -> [Position]
activePos piece = map (addPos $ piece^.pos) $ shape (piece^.pieceType) (piece^.orientation)
