module Team (
      Team ( Team, name, players )
    ) where

import Player

data Team = Team {
      name :: String
    , players :: [Player]
    }
