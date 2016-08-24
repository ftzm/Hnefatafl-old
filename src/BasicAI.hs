{-# LANGUAGE TupleSections #-}
module BasicAI where

--import BoardData
import Engine
import BoardData
import qualified Data.Map as M

import Data.Ord
import Data.Maybe
import Data.List
import Control.Monad

import qualified Data.IntSet as S

pickStrategy :: GameState -> (GameState -> Moves -> PostTurn)
pickStrategy g = if whiteTurn g then whiteStrategy else blackStrategy

whiteStrategy :: GameState -> Moves -> PostTurn
whiteStrategy g m = bestMove rateWhite (Right m,g)

blackStrategy :: GameState -> Moves -> PostTurn
blackStrategy g m = bestMove rateBlack (Right m,g)

cornerGuards :: [[Coord]]
cornerGuards = map (map xyToInt) [[(0,2),(1,1),(2,0)],[(8,0),(9,1),(10,2)],[(0,8),(1,9),(2,10)]
               ,[(8,10),(9,9),(10,8)]]

-- |maybe message about fuckyness
-- |Award 20 points for each piece taken, take 20 for each lost
--  Range: -60 - 60
ratioBalanced :: GameState -> Int
ratioBalanced g | whiteTurn g = 20 * (whiteLosses g - blackLosses g)
                | otherwise   = 20 * (blackLosses g - whiteLosses g)
------------------------------------------------------------
--- Pathfinding
------------------------------------------------------------

discoverCoords :: S.IntSet -> [(Direction, [Coord])] -> S.IntSet
discoverCoords = foldl' (\acc (_,x:_) -> S.insert x acc)

uniqueSndHead :: Eq b => [(a,[b])] -> [(a,[b])]
uniqueSndHead = nubBy (\x y -> head (snd x)==head (snd y))

noDiscovered :: S.IntSet -> [(Direction, [Coord])] -> [(Direction, [Coord])]
noDiscovered ds = filter (\(_,x:_) -> S.notMember x ds)

startTree :: Board -> Square -> ([(Direction, [Coord])],S.IntSet)
startTree b s = (routes,discovered)
  where
    routes = concatMap (\x -> map (\y -> (x,[y])) (dirMoves b s x)) dirs
    discovered = discoverCoords S.empty routes

newBranching :: Board -> Coord -> Piece -> Direction -> [(Direction,Coord)]
newBranching b c p d = concatMap (\x -> map (\y -> (x,y)) (dirMoves b (c,p) x)) $ perp d

expandTree' :: Board -> Piece -> ([(Direction, [Coord])],S.IntSet) -> ([(Direction, [Coord])],S.IntSet)
expandTree' b p (xs,ds) = (trimmedCoords,newDiscovered)
 where
   newCoords = concatMap (\(x,ys) -> map (\(k,i) -> (k,i:ys)) (newBranching b (head ys) p x)) xs
   trimmedCoords = noDiscovered ds $ uniqueSndHead newCoords
   newDiscovered = discoverCoords ds trimmedCoords

foundRoute :: Coord -> ([(Direction, [Coord])],S.IntSet) -> Maybe [Coord]
foundRoute g r = find ((g==) . head) $ map snd $ fst r

findRoute :: Board -> Square -> Coord -> Maybe [Coord]
findRoute b s@(_,p) g
  | isNothing found = searchWider b p g initialSearch
  | otherwise = found
    where
      initialSearch = startTree b s
      found = foundRoute g initialSearch

searchWider :: Board -> Piece -> Coord -> ([(Direction, [Coord])],S.IntSet) -> Maybe [Coord]
searchWider b p g r
  | isJust found = found
  | exhausted = Nothing
  | otherwise = searchWider b p g result
    where
      result = expandTree' b p r
      exhausted = null $ fst result
      found = foundRoute g result

------------------------------------------------------------
------------------------------------------------------------


kingEscapeMoves :: GameState -> Int
kingEscapeMoves g
  | null moveNums = 0
  | otherwise = sum $ map (\x -> 15-x) $ filter (<5) moveNums
  where
    b = board g
    kingSquare = (king $ board g,King)
    moveNums = map length $ mapMaybe (findRoute b kingSquare) cornerCoords

kingSurrounding ::
--kingsurrounding =

enemiesAroundKing :: GameState -> Int


--  Range: -2 - 3
rateCorners :: GameState -> Int
rateCorners g = score s2 - score s1
  where
    (s1,s2) = lastMove g
    score = maybe 0 rating . inGuard
    rating = length . filter (sEq s1) . map (getSquare (board g))
    inGuard s = find (elem $ fst s) cornerGuards

-- |How many foes can move to a given empty square in one move?
--  automatically 10 points if any, plus the number of foes.
--  Range: 0-13
foesInRange :: GameState -> (Square -> Bool) -> Square -> Int
foesInRange g f s = bonus . length $ filter f $  mapMaybe (findNextPiece (board g) s) dirs
  where bonus x | x > 0 = 10 + x
                | otherwise = 0
 --
    --relDirs = delete d dirs

-- |Given a board, square and direction, if the next square over is a foe
--  and the one behind is empty, return the empty square.
vulnBehind :: Board -> Square -> Direction -> Maybe Square
vulnBehind b s d = ifMaybe (fromJust s2) =<< liftM2 (&&)
                   (foes s <$> s1) ((==Empty) . snd <$> s2)
  where
    s1 = go b s d
    s2 = flip (go b) d =<< s1

vulnHere :: Board -> Square -> Direction -> Bool
vulnHere b s d = fromMaybe False $ liftM2 (&&) (friends s <$> s1) (liftM2 foes s1 s2)
  where
    s1 = go b s d
    s2 = flip (go b) d =<< s1

-- |Given a square, see if any surrounding squares are threatened by it
--  Range: 0 - 39 (13 per)
threatenOther :: GameState -> Int
threatenOther g = sum $ map (foesInRange g (friends s)) $ mapMaybe (vulnBehind b s) dirs
  where
    b = board g
    s = snd $ lastMove g

-- |Given a square, see if it is threatened by surrounding squares.
--  Range: 0 - 39
arrivalRisk :: GameState -> Int
arrivalRisk g = sum
                 $ map (foesInRange g (mobileFoes s))
                 $ filter ((==Empty) . snd)
                 $ mapMaybe (go b s . opp)
                 $ filter (maybe False (foes s) . go b s) dirs
  where b = board g
        s = snd $ lastMove g

--  Range: 0 - 13
vacateRisk :: GameState -> Int
vacateRisk g = if any (vulnHere b s) dirs then foesInRange g (foes s) s else 0
 where b = board g
       s = fst $ lastMove g

allGameStates :: GameState -> Moves -> [PostTurn]
allGameStates g = let movesList = M.foldlWithKey (\a k x -> map (k,) (concat x) ++ a) []
                  in map (runTurn g) . movesList

rateWhite :: Either WinLose Moves -> GameState -> Int
rateWhite (Right _) g = ratioBalanced g
rateWhite (Left w) _
  | KingCapture <- w = -100
  | NoMoves     <- w =  100
  | NoPieces    <- w =  100
  | Escape      <- w =  100

blackConcerns :: [(GameState -> Int , Int -> Int , Int -> Int -> Int)]
blackConcerns =
  [(whiteLosses,     (*100),  (+))
  ,(threatenOther,   id,      (+))
  ,(rateCorners,     id,      (+))
  ,(arrivalRisk,     (*100000), (-))
  ,(vacateRisk,      id,      (-))
  ,(kingEscapeMoves, (*1000), (-))
  ]

evalConcerns :: GameState
             -> [(GameState -> Int , Int -> Int , Int -> Int -> Int)]
             -> Int
evalConcerns g = foldl' (\acc (c,a,m) -> m acc (a $ c g)) 0

rateBlack :: Either WinLose Moves -> GameState -> Int
rateBlack (Right _) g = evalConcerns g blackConcerns
rateBlack (Left w) _
  | KingCapture <- w =  100
  | NoMoves     <- w =  100
  | Escape      <- w = -100

bestMove :: (Either WinLose Moves -> GameState -> Int) -> PostTurn -> PostTurn
bestMove _ (Left m,g) = (Left m,g)
bestMove r (Right m,g) = maximumBy (comparing (uncurry r)) $ allGameStates g m

generateMove :: GameState -> Moves -> PostTurn
generateMove g = pickStrategy g g
