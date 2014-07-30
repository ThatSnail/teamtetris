{-# LANGUAGE TemplateHaskell #-}

module Game (
    ) where

import Control.Lens
import System.Random

import Player
import Board

type Team = [Player]

data Game = Game {
      _boards :: [Board]
    , _teams :: [Team]
    }
makeLenses ''Game

updateGame :: Game -> Game
updateGame game = game & boards %~ map updateBoard

type TeamCount = Int
type Seed = StdGen

makeGame :: Seed -> TeamCount -> Game
makeGame seed teamCount = Game boards teams
    where
        boards = makeBoard
        teams = replicate teamCount makeTeam 
