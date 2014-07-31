import Haste
import Haste.Graphics.Canvas

import Control.Monad
import Control.Applicative
import Lens.Family2
--import Control.Lens
--import System.Random

import Game
import Board
import BoardDimensions
import Utils

canvasID :: ElemID
canvasID = "canvas"

tileWidth :: Double
tileWidth = 20

tileHeight :: Double
tileHeight = 20

drawTile :: Board -> Point2D Int -> Picture ()
drawTile board (Point2D x y) = color c $ fill $ rect (tx + 2, ty + 2) (tx + tileWidth - 2, ty + tileHeight - 2)
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
    seed <- newSeed
    let game = makeGame seed 2 2
    Just canvas <- getCanvasById canvasID
    render canvas $ do drawBoard ((game^.boards) !! 0) (0, 0)
                       drawBoard ((game^.boards) !! 1) (150, 0)
