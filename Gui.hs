import Haste
import Haste.Graphics.Canvas

import Control.Monad
import Control.Applicative

import Board
import BoardDimensions
import Utils

canvasID :: ElemID
canvasID = "canvas"

tileWidth :: Double
tileWidth = 10

tileHeight :: Double
tileHeight = 10

drawTile :: Board -> Point2D Int -> Picture ()
drawTile board (Point2D x y) = color c $ fill $ rect (tx, ty) (tx + tileWidth, ty + tileHeight)
    where
        tx = fromIntegral x * tileWidth
        ty = fromIntegral y * tileHeight
        c
          | isOccupied board x y = RGB 0 0 0
          | otherwise            = RGB 128 128 128

drawBoard :: Board -> Point -> Picture ()
drawBoard board (bx, by) = sequence_ tilePics
    where
        tilePics = map (drawTile board) pointList
            where
                pointList = liftA2 Point2D (map fromIntegral [0..boardWidth]) (map fromIntegral [0..boardHeight])

main :: IO ()
main = do
    Just canvas <- getCanvasById canvasID
    render canvas $ drawBoard emptyBoard (0, 0)
