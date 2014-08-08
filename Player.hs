module Player (
      Player ( Player )
    , name
    ) where

import Lens.Family2
import Lens.Family2.Unchecked

data Player = Player {
      _name :: String
    }

name :: Lens Player Player String String
name = lens _name (\s x -> s { _name = x })
