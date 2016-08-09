{-# LANGUAGE TupleSections #-}
module BasicAI where

--import BoardData
import Engine
import BoardData
import Data.Maybe
import qualified Data.Map as M

import Data.Ord
import Data.List

pickStrategy :: GameState -> (GameState -> Moves -> (Either WinLose Moves, GameState))
pickStrategy g = if whiteTurn g then blackStrategy else blackStrategy

--whiteStrategy :: GameState -> (Either WinLose Moves, GameState)
--whiteStrategy g = g

blackStrategy :: GameState -> Moves -> (Either WinLose Moves, GameState)
blackStrategy g m = bestMove 0 $ allGameStates g m

--escapeMoves :: Board -> Int

cornerGuards :: [[Coord]]
cornerGuards = [[(0,2),(1,1),(2,0)],[(8,0),(9,1),(10,2)],[(0,8),(1,9),(2,10)]
               ,[(8,10),(9,9),(10,8)]]

--maybe message about fuckyness
ratioBalanced :: GameState -> Int
ratioBalanced g | whiteTurn g = whiteLosses g - blackLosses g
                | otherwise   = blackLosses g - whiteLosses g

rateCorners :: Board -> (Square, Square) -> Int
rateCorners b (s1,s2) = score s2 - score s1
  where
    score = maybe 0 rating . inGuard
    rating = length . filter (sEq s1) . map (fromJust . flip getSquare b)
    inGuard s = find (elem $ fst s) cornerGuards

allGameStates :: GameState -> Moves -> [(Either WinLose Moves, GameState)]
allGameStates g = let movesList = M.foldlWithKey (\a k x -> (map (k,) x) ++ a) []
                  in map (runTurn g) . movesList

rateWhite :: GameState -> Int
rateWhite = ratioBalanced

rateBlack :: Either WinLose Moves -> GameState -> Int
rateBlack (Right m) g = ratioBalanced g
rateBlack (Left w) g = ratioBalanced g

bestMove :: Int -> [(Either WinLose Moves, GameState)] -> (Either WinLose Moves, GameState)
bestMove i
  | i == 0 = maximumBy (comparing (uncurry rateBlack))
  -- | otherwise = best (comparing (ratio . bestMove (i-1) . bestMove 0)) $ allMoves g
  --where
   -- best = if frontTurn g then maximumBy else minimumBy

generateMove :: GameState -> Moves -> (Either WinLose Moves, GameState)
generateMove g m = (pickStrategy g) g m
