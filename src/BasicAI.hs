module BasicAI where

--import BoardData
import Engine

import Data.Ord
import Data.List

bestMove :: Int -> GameState -> GameState
bestMove i g
  | i == 0 = best (comparing ratio) $ allMoves g
  | otherwise = best (comparing (ratio . bestMove (i-1) . bestMove 0)) $ allMoves g
  where
    best = if frontTurn g then maximumBy else minimumBy

generateMove :: GameState -> GameState
generateMove g = bestMove 1 g
