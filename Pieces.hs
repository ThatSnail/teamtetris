-- Tile data based on http://colinfahey.com/tetris/tetris_diagram_pieces_orientations_new.jpg

module Pieces (
      PieceType
    , Orientation ( First )
    , shape
    , color
    , randomPieceType
    ) where

import Haste.Graphics.Canvas ( Color ( RGB ) )

import Haste.App
import Utils

data PieceType = O | I | S | Z | L | J | T deriving (Enum, Bounded)

data Orientation = First | Second | Third | Fourth deriving Eq

shape :: PieceType -> Orientation -> [Position]
shape O _ = [(0, 0), (0, -1), (-1, -1), (-1, 0)]
shape I o
    | o == First  || o == Third  = [(-2, 0), (-1, 0), (0, 0), (1, 0)]
    | o == Second || o == Fourth = [(0, -2), (0, -1), (0, 0), (0, 1)]
shape S o
    | o == First  || o == Third  = [(-1, -1), (0, -1), (0, 0), (1, 0)]
    | o == Second || o == Fourth = [(0, 1), (0, 0), (1, 0), (1, -1)]
shape Z o
    | o == First  || o == Third  = [(-1, 0), (0, 0), (0, -1), (1, -1)]
    | o == Second || o == Fourth = [(0, -1), (0, 0), (1, 0), (1, 1)]
shape L o
    | o == First                 = [(-1, -1), (-1, 0), (0, 0), (1, 0)]
    | o == Second                = [(1, -1), (0, -1), (0, 0), (0, 1)]
    | o == Third                 = [(-1, 0), (0, 0), (1, 0), (1, 1)]
    | o == Fourth                = [(-1, 1), (0, 1), (0, 0), (0, -1)]
shape J o
    | o == First                 =  [(-1, 0), (0, 0), (1, 0), (1, -1)]
    | o == Second                = [(0, -1), (0, 0), (0, 1), (1, 1)]
    | o == Third                 = [(-1, 1), (-1, 0), (0, 0), (1, 0)]
    | o == Fourth                = [(-1, -1), (0, -1), (0, 0), (0, 1)]
shape T o
    | o == First                 = [(-1, 0), (0, 0), (0, -1), (1, 0)]
    | o == Second                = [(0, -1), (0, 0), (0, 1), (1, 0)]
    | o == Third                 = [(-1, 0), (0, 0), (0, 1), (1, 0)]
    | o == Fourth                = [(-1, 0), (0, -1), (0, 0), (0, 1)]

color :: PieceType -> Color
color O = RGB 255 255 0
color I = RGB 0 255 255
color S = RGB 0 255 0
color Z = RGB 255 0 0
color L = RGB 0 0 255
color J = RGB 255 128 0
color T = RGB 255 0 255

randomPieceType :: Seed -> (PieceType, Seed)
randomPieceType g = (pieceList !! pInd, ng)
    where
        pieceList = [(minBound :: PieceType)..]
        (pInd, ng) = randomR (0, (length pieceList) - 1) g
