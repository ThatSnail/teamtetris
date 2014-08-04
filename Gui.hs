import Haste
import Haste.Graphics.Canvas

import Data.Maybe
import Control.Monad
import Control.Applicative
import Lens.Family2
--import Control.Lens
--import System.Random

import ActivePiece
import qualified Pieces
import Game
import Board
import BoardState
import BoardDimensions
import Utils

canvasID :: ElemID
canvasID = "canvas"

tileWidth :: Double
tileWidth = 20

tileHeight :: Double
tileHeight = 20

drawTile :: Board -> Position -> Picture ()
drawTile board (x, y) = color c $ fill $ rect (tx + 2, ty + 2) (tx + tileWidth - 2, ty + tileHeight - 2)
    where
        tx = fromIntegral x * tileWidth
        ty = fromIntegral (boardHeight - y - 1) * tileHeight
        c
          | isOccupied (board^.state) x y           = RGB 0 0 0
          | isJust mActivePiece                     = Pieces.color $ (fromJust mActivePiece)^.pieceType
          | otherwise                               = RGB 128 128 128
            where
                mActivePiece = getActiveAtPos board x y

drawBoard :: Board -> Point -> Picture ()
drawBoard board (bx, by) = sequence_ tilePics
    where
        tilePics = map (translate (bx, by) . drawTile board) pointList
            where
                pointList = liftA2 (,) (map fromIntegral [0..boardWidth-1]) (map fromIntegral [0..boardHeight-1])

main :: IO ()
main = do
    seed <- newSeed
    Just canvas <- getCanvasById canvasID

    let initGame = makeGame seed 2 2
    let draw game = render canvas $ do drawBoard ((game^.boards) !! 0) (0, 0)
                                       drawBoard ((game^.boards) !! 1) (300, 0)
    let updateClient game = do (return $ updateGame game)
                               draw $ updateGame game
                               setTimeout 20 $ updateClient (updateGame game)

    updateClient initGame
