module Team (
      Team ( Team, players )
    , emptyTeam
    ) where

import Player

data Team = Team {
      players :: [Player]
    }

emptyTeam :: Team
emptyTeam = Team []
