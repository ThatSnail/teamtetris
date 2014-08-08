module Client (
    ) where

import Haste
import Haste.Ajax hiding (Key)
import Haste.Graphics.Canvas
import Lens.Family2
import Control.Monad.IO.Class
import Data.Maybe

import Gui
import Game
import Utils
import Player
import Server

canvasID :: ElemID
canvasID = "canvas"

type KeyCode = Int

data Key = Key_Left | Key_Up | Key_Right | Key_Down deriving Show

toKey :: KeyCode -> Key
toKey 37 = Key_Left
toKey 38 = Key_Up
toKey 39 = Key_Right
toKey 40 = Key_Down

sendMove :: (MonadIO m) => Position -> m ()
sendMove (x, y) = textRequest POST "./move.php" [
      ("dx", show x)
    , ("dy", show y)
    ] recGame

recGame :: Maybe String -> IO ()
recGame s = do
    alert (fromJust s)

handleKey :: Key -> IO ()
handleKey key = do
    alert ("Key pressed!" ++ show key)

-- Ask the server for a game, pretty please?
askForGame :: Player -> IO (Maybe ServerGame)
askForGame player = do
    textRequest POST "./retrieveGame.php" [("playerName", player^.name)] onGameRetrieve

onGameRetrieve :: Maybe String -> IO ()
onGameRetrieve mGameStr = 

main :: IO ()
main = do
    playerName <- prompt "Please enter your name:"
    let player = Player playerName
    askForGame player
--    game = serverGame
{-|
    Just canvas <- getCanvasById canvasID
    Just inp <- elemById "body"
    Just game <- getCurrentGame

    let handle game = onEvent inp OnKeyDown $ \keyInt -> do
                        handleKey (toKey keyInt)
                        return ()
    let draw game = render canvas $ drawGame game
    let updateClient game = do (return $ updateGame game)
                               draw $ updateGame game
                               setTimeout 100 $ updateClient (updateGame game)
    alert "Hello!"
    --updateClient initGame
|-}
