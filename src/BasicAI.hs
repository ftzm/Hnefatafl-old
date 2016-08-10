{-# LANGUAGE TupleSections #-}
module BasicAI where

--import BoardData
import Engine
import BoardData
import Data.Maybe
import qualified Data.Map as M

import Data.Ord
import Data.List

pickStrategy :: GameState -> (GameState -> Moves -> PostTurn)
pickStrategy g = if whiteTurn g then whiteStrategy else blackStrategy

whiteStrategy :: GameState -> Moves -> PostTurn
whiteStrategy g m = bestMove 1 (rateWhite,rateBlack) (Right m,g)

blackStrategy :: GameState -> Moves -> PostTurn
blackStrategy g m = bestMove 1 (rateBlack,rateWhite) (Right m,g)

--escapeMoves :: Board -> Int

cornerGuards :: [[Coord]]
cornerGuards = [[(0,2),(1,1),(2,0)],[(8,0),(9,1),(10,2)],[(0,8),(1,9),(2,10)]
               ,[(8,10),(9,9),(10,8)]]

--maybe message about fuckyness
ratioBalanced :: GameState -> Int
ratioBalanced g | whiteTurn g = 4 * whiteLosses g - blackLosses g
                | otherwise   = 4 * blackLosses g - whiteLosses g

rateCorners :: GameState -> Int
rateCorners g = score s2 - score s1
  where
    (s1,s2) = lastMove g
    score = maybe 0 rating . inGuard
    rating = length . filter (sEq s1) . map (fromJust . flip getSquare (board g))
    inGuard s = find (elem $ fst s) cornerGuards

allGameStates :: GameState -> Moves -> [PostTurn]
allGameStates g = let movesList = M.foldlWithKey (\a k x -> map (k,) x ++ a) []
                  in map (runTurn g) . movesList

rateWhite :: Either WinLose Moves -> GameState -> Int
rateWhite (Right _) g = ratioBalanced g
rateWhite (Left w) _
  | KingCapture <- w = -100
  | NoMoves <- w = 100
  | NoPieces <- w = 100
  | Escape <- w = 100

rateBlack :: Either WinLose Moves -> GameState -> Int
rateBlack (Right _) g = ratioBalanced g + rateCorners g
rateBlack (Left w) _
  | KingCapture <- w = 100
  | NoMoves <- w = 100
  | Escape <- w = -100

bestMove :: Int
         -> (Either WinLose Moves -> GameState -> Int, -- the rating strategy for the AI team
            Either WinLose Moves -> GameState -> Int) -- the rating strategy the AI will
                                                        -- asssume from the opposing team
         -> PostTurn -- list of all possible PostTurns
         -> PostTurn -- the selected PostTurn
bestMove _ _ (Left m,g) = (Left m,g)
bestMove 0 (r1,_) (Right m,g) = maximumBy (comparing (uncurry r1)) $ allGameStates g m
bestMove i (r1,r2) (Right m,g) = maximumBy (comparing (uncurry r1) . bestMove (i-1) (r1,r2) . bestMove 0 (r2,r1) ) $ allGameStates g m

generateMove :: GameState -> Moves -> PostTurn
generateMove g m = pickStrategy g g m
