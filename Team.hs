module Team (
      Team ( Team, players )
    ) where

import Player

data Team = Team {
      players :: [Player]
    }

makeTeam :: Team
makeTeam = Team []
