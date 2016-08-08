module BasicAI where

--import BoardData
import Engine
import BoardData
import Data.Maybe

--import Data.Ord
import Data.List

--escapeMoves :: Board -> Int

cornerGuards :: [[Coord]]
cornerGuards = [[(0,2),(1,1),(2,0)],[(8,0),(9,1),(10,2)],[(0,8),(1,9),(2,10)]
               ,[(8,10),(9,9),(10,8)]]

rateCorners :: Board -> (Square, Square) -> Int
rateCorners b (s1,s2) = score s2 - score s1
  where
    score = maybe 0 rating . inGuard
    rating = length . filter (sEq s1) . map (fromJust . flip getSquare b)
    inGuard s = find (elem $ fst s) cornerGuards

--bestMove :: Int -> GameState -> GameState
--bestMove i g
--  | i == 0 = best (comparing ratio) $ allMoves g
--  | otherwise = best (comparing (ratio . bestMove (i-1) . bestMove 0)) $ allMoves g
--  where
--    best = if frontTurn g then maximumBy else minimumBy
--
--generateMove :: GameState -> GameState
--generateMove g = bestMove 1 g
