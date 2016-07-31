module BasicAI where

--import BoardData
import Engine

import Data.Ord
import Control.Arrow
import Data.List

import qualified Data.IntMap.Strict as IM

allMoves :: GameState -> [GameState]
allMoves g = map (movePiece g) $ concatMap (pieceMoves (board g) . first intToCoord) squares
  where
    squares
      | frontTurn g = IM.assocs $ IM.filter whitePiece $ board g
      | otherwise = IM.assocs $ IM.filter blackPiece $ board g

bestMove :: Int -> GameState -> GameState
bestMove i g
  | i == 0 = best (comparing ratio) $ allMoves g
  | otherwise = best (comparing (ratio . bestMove (i-1) . bestMove 0)) $ allMoves g
  where
    best = if frontTurn g then maximumBy else minimumBy

generateMove :: GameState -> GameState
generateMove g = bestMove 1 g
