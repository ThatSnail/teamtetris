module Board (
      Board ( Board )
    , isOccupied
    , updateBoard
    ) where

import Team
import ActivePiece
import BoardDimensions
import Utils

data TileState = Empty | Occupied deriving Eq
type State = [[TileState]]

data Board = Board { 
      state :: State
    , team :: Team
    , activePieces :: [ActivePiece]
    , spawnPoints :: [Position]
    , seed :: StdGen
    }

isOccupied :: Board -> Int -> Int -> Bool
isOccupied board x y = ((state board) !! x !! y) == Occupied

emptyState :: State
emptyState = replicate boardHeight $ replicate boardWidth Empty

updateBoard :: Board -> Board
updateBoard board@(Board state _ activePieces spawnPoints seed) = foldr f board { activePieces = [] } $ zipWith activePieces spawnPoints
    where
        -- f :: Board -> (activePiece :: ActivePiece, spawnPoint :: Position)
        f board@(Board state _ aps _ seed) (ap sp)
            | snd nap == False = board { activePieces = nap : aps }
            | otherwise        = board { state = addPieceToState aps state, activePieces = (nap { pos = sp, pieceType = fst randomPieceType seed, orientation = First } ) : aps, seed = snd randomPieceType seed }
                where
                    nap = updatePiece ap

addPieceToState :: ActivePiece -> State -> State
addPieceToState (ActivePiece _ (px, py) pieceType orientation) state = foldr addPosToState state $ map (\(x, y) -> (x + px, y + py)) $ shape pieceType orientation

addPosToState :: Position -> State -> State
addPosToState (x, y) state = state !! x !! y = Occupied
