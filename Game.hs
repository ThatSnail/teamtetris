--{-# LANGUAGE TemplateHaskell #-}

module Game (
      makeGame
    , updateGame
    , boards
    ) where

import Lens.Family2
import Lens.Family2.Unchecked
--import Control.Lens
import Haste
--import System.Random

import Player
import Team
import Board
import BoardDimensions

data Game = Game {
      _boards :: [Board]
    , _teams :: [Team]
    }
--makeLenses ''Game

boards :: Lens Game Game [Board] [Board]
boards = lens _boards (\s x -> s { _boards = x })

updateGame :: Game -> Game
updateGame game = game & boards %~ map updateBoard

type TeamCount = Int
type PlayersPerTeam = Int
--type Seed = StdGen

makeGame :: Seed -> TeamCount -> PlayersPerTeam -> Game
makeGame seed teamCount playersPerTeam = Game boards teams
    where
        spawnPoints = spawns playersPerTeam
        boards = replicate teamCount $ makeBoard spawnPoints seed
        teams = replicate teamCount emptyTeam 
