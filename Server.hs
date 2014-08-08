module Server (
      ServerGame ( ServerGame )
    , game
    ) where

import Lens.Family2
import Lens.Family2.Unchecked

import Game

data ServerGame = ServerGame {
      _gameID :: Int
    , _game :: Game
    }

game :: Lens ServerGame ServerGame Game Game
game = lens _game (\s x -> s { _game = x })

{-|
generateGame :: ServerGame
generateGame 

main :: IO ()
main = do
    let initGame = makeGame seed 2 2
|-}
