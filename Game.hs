module Game (
    ) where

import Player

type Team = [Player]

data Game = Game {
      boards :: [Board]
    , teams :: [Team]
    }

updateGame :: Game -> Game
updateGame game@(Game { boards = bs }) = game { boards = map updateBoard bs }
