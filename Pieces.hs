-- Tile data based on http://colinfahey.com/tetris/tetris_diagram_pieces_orientations_new.jpg

module Pieces (
      shape
    , color
    ) where

import Haste.Graphics.Canvas

data Tile = Tile Int Int
type TileDelta = Tile

data Piece = O | I | S | Z | L | J | T

shape :: Piece -> [[TileDelta]]
shape O = [
      [(0, 0), (0, -1), (-1, -1), (-1, 0)]
    ]
shape I = [
      [(-2, 0), (-1, 0), (0, 0), (1, 0)]
    , [(0, -2), (0, -1), (0, 0), (0, 1)]
    ]
shape S = [
      [(-1, -1), (0, -1), (0, 0), (1, 0)]
    , [(0, 1), (0, 0), (1, 0), (1, -1)]
    ]
shape Z = [
      [(-1, 0), (0, 0), (0, -1), (1, -1)]
    , [(0, -1), (0, 0), (1, 0), (1, 1)]
    ]
shape L = [
      [(-1, -1), (-1, 0), (0, 0), (1, 0)]
    , [(1, -1), (0, -1), (0, 0), (0, 1)]
    , [(-1, 0), (0, 0), (1, 0), (1, 1)]
    , [(-1, 1), (0, 1), (0, 0), (0, -1)]
    }
shape J = [
      [(-1, 0), (0, 0), (1, 0), (1, -1)]
    , [(0, -1), (0, 0), (0, 1), (1, 1)]
    , [(-1, 1), (-1, 0), (0, 0), (1, 0)]
    , [(-1, -1), (0, -1), (0, 0), (0, 1)]
    ]
shape T = [
      [(-1, 0), (0, 0), (0, -1), (1, 0)]
    , [(0, -1), (0, 0), (0, 1), (1, 0)]
    , [(-1, 0), (0, 0), (0, 1), (1, 0)]
    , (-1, 0), (0, -1), (0, 0), (0, 1)]
    ]

color :: Piece -> Color
color O = Color 255 255 0
color I = Color 0 255 255
color S = Color 0 255 0
color Z = Color 255 0 0
color L = Color 0 0 255
color J = Color 255 128 0
color T = Color 255 0 255
