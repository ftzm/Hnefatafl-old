{-# LANGUAGE TupleSections #-}

module BasicAI
 (generateMove,
  allGameStates
 )
where

import Board
import Moves
import Turns
import Capture
import GameState

import qualified Data.Map as M
import Data.Ord
import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative
import Control.Arrow
import qualified Data.IntSet as S

dirs :: [Direction]
dirs = [North,East,South,West]

mobileFoes :: Square -> Square -> Bool
mobileFoes (_,p1) (_,p2)
  | whitePiece p1 = blackPiece p2
  | blackPiece p1 = whitePiece p2
  | otherwise    = False

-- ----------------------------------------------------------------------------
-- Pathfinding

type Trail = (Direction, [Coord])
type SearchProgress = ([Trail], S.IntSet)

-- |Given a [(Direction, [Coord])] list, add new head coords to
--  the list of discovered coordinates
discoverCoords :: S.IntSet -> [Trail] -> S.IntSet
discoverCoords = foldl' (\acc (_,x:_) -> S.insert x acc)

removeDiscovered :: S.IntSet -> [Trail] -> [Trail]
removeDiscovered ds = filter (\(_,x:_) -> S.notMember x ds)

newSearch :: Board -> Square -> SearchProgress
newSearch b s = id &&& discoverCoords S.empty
              $ concatMap (\x -> map (\y -> (x,[y])) (dirMoves b s x)) dirs

newMoves :: Board -> Piece -> Trail -> [Trail]
newMoves b p (d,cs@(c:_)) = concatMap f $ perp d
  where f x = map ((x, ) . (: cs)) (dirMoves b (c, p) x)
newMoves _ _ t = [t]

growProg :: Board -> Piece -> SearchProgress -> SearchProgress
growProg b p (xs,ds) = id &&& discoverCoords ds
                       $ removeDiscovered ds $ concatMap (newMoves b p) xs

route :: Coord -> SearchProgress -> Maybe [Coord]
route g = find ((g ==) . head) . map snd . fst

search :: Board -> Piece -> SearchProgress -> Coord -> Maybe [Coord]
search _ _ ([],_) _ = Nothing
search b p pr g = route g pr <|> search b p (growProg b p pr) g

findRoute :: Board -> Square -> Coord -> Maybe [Coord]
findRoute b s@(_,p) = search b p (newSearch b s)

allDestinations :: Board -> Square -> Int
allDestinations b s@(_,p) = S.size $ snd $ last
                          $ takeWhileIncl (not . null . fst)
                          $ iterate (growProg b p)
                          $ newSearch b s

-- ----------------------------------------------------------------------------
-- Board Judging Methods

sqr :: Int -> Int
sqr x = x*x

kingEscapeMoves :: GameState -> Int
kingEscapeMoves g = sum $ map (\x -> sqr $ 5 - x) $ filter (< 5) moveNums
  where
    b = board g
    kingSquare = (king $ board g, King)
    moveNums = map length $ mapMaybe (findRoute b kingSquare) cornerCoords

enemiesAroundKing :: GameState -> Int
enemiesAroundKing g = length $ filter (foes s) $ mapMaybe (go b s) dirs
  where
    b = board g
    s = getSquare b $ king b

cornerGuards :: [[Coord]]
cornerGuards = map (map xyToInt)
        [ [(0, 2), (1, 1), (2, 0)]
        , [(8, 0), (9, 1), (10, 2)]
        , [(0, 8), (1, 9), (2, 10)]
        , [(8, 10), (9, 9), (10, 8)]]

--  Range: -2 - 3
rateCorners :: GameState -> Int
rateCorners g = score s2 - score s1
  where
    (s1,s2) = lastMove g
    score = maybe 0 rating . inGuard
    rating = length . filter (sEq s1) . map (getSquare (board g))
    inGuard s = find (elem $ fst s) cornerGuards

-- |How many foes can move to a given empty square in one move?
-- automatically 10 points if any, plus the number of foes.
-- Range: 0-13
foesInRange :: GameState -> (Square -> Bool) -> Square -> Int
foesInRange g f s = bonus . length $ filter f
                  $ mapMaybe (findNextPiece (board g) s) dirs
  where bonus x | x > 0 = 10 + x
                | otherwise = 0

-- |Given a board, square and direction, if the next square over is a foe
-- and the one behind is empty, return the empty square.
vulnBehind :: Board -> Square -> Direction -> Maybe Square
vulnBehind b s d = ifMaybe (fromJust s2)
                 =<< liftM2 (&&) (foes s <$> s1) ((== Empty) . snd <$> s2)
  where s1 = go b s d
        s2 = flip (go b) d =<< s1

--enemyBehind :: Board -> Square -> Direction -> Bool
--enemyBehind b s d = fromMaybe False (foes s <$> go b s d)

-- |Given a board, square, and direction, if the next square over in d is a friend and the square behind is an enemy, then return true.
vulnHere :: Board -> Square -> Direction -> Bool
vulnHere b s d =
    fromMaybe False $ liftM2 (&&) (friends s <$> s1) (liftM2 foes s1 s2)
  where
    s1 = go b s d
    s2 = flip (go b) d =<< s1

-- |Given a square, see if any surrounding squares are threatened by it
-- Range: 0 - 39 (13 per)
threatenOther :: GameState -> Int
threatenOther g = sum $ map (foesInRange g (friends s))
                $ mapMaybe (vulnBehind b s) dirs
  where
    b = board g
    s = snd $ lastMove g

-- |Given a square, see if it is threatened by surrounding squares.
-- Range: 0 - 39
arrivalRisk :: GameState -> Int
arrivalRisk g
  | snd s == King = 0
  | otherwise = sum
              $ map (foesInRange g (mobileFoes s))
              $ filter ((== Empty) . snd)
              $ mapMaybe (go b s . opp)
              $ filter (maybe False (foes s) . go b s) dirs
  where
    b = board g
    s = snd $ lastMove g

-- |
--  Range: 0 - 13
vacateRisk :: GameState -> Int
vacateRisk g = if any (vulnHere b s) dirs then foesInRange g (foes s) s else 0
  where b = board g
        s = fst $ lastMove g

-- |The ammount of squares available to the king to move to given unlimited
-- moves. Encourages the AI to close in on the king.
moveRoom :: GameState -> Int
moveRoom g = allDestinations b kingSquare
  where b = board g
        kingSquare = (king $ board g, King)



-- ----------------------------------------------------------------------------
-- Judge Boards

pickStrategy :: GameState -> (GameState -> Moves -> PostTurn)
pickStrategy g = if whiteTurn g then whiteStrategy else blackStrategy

whiteStrategy :: GameState -> Moves -> PostTurn
whiteStrategy g m = bestMove rateWhite (Right m, g)

blackStrategy :: GameState -> Moves -> PostTurn
blackStrategy g m = bestMove rateBlack (Right m, g)

allGameStates :: GameState -> Moves -> [PostTurn]
allGameStates g = map (runTurn g) . movesList
  where movesList = M.foldlWithKey (\a k x -> map (k, ) (concat x) ++ a) []

type RateAngle = (GameState -> Int, Int -> Int, Int -> Int -> Int)

blackConcerns :: [RateAngle]
blackConcerns =
    [ (whiteLosses,       (* 100),    (+))
    , (threatenOther,     id,         (+))
    , (rateCorners,       id,         (+))
    , (enemiesAroundKing, (* 20),     (+))
    , (arrivalRisk,       (* 100000), (-))
    , (vacateRisk,        id,         (-))
    , (kingEscapeMoves,   (* 1000),   (-))
    , (moveRoom,          (* 10),     (-))
    ]

whiteConcerns :: [RateAngle]
whiteConcerns =
    [ (blackLosses,       (* 1000),      (+))
    , (threatenOther,     id,           (+))
    , (kingEscapeMoves,   (* 100000), (+))
    , (moveRoom,          (* 10),       (+))
    , (enemiesAroundKing, id,           (-))
    , (arrivalRisk,       (* 100),     (-))
    , (vacateRisk,        id,           (-))
    ]

evalConcerns :: GameState -> [RateAngle] -> Int
evalConcerns g = foldl' (\acc (c,a,m) -> m acc (a $ c g)) 0

rateBlack :: Either WinLose Moves -> GameState -> Int
rateBlack (Right _) g = evalConcerns g blackConcerns
rateBlack (Left w) _
  | KingCapture <- w =  100000000
  | NoMoves     <- w =  100000000
  | Escape      <- w = -1000000000000
  | _           <- w =  0

rateWhite :: Either WinLose Moves -> GameState -> Int
rateWhite (Right _) g = evalConcerns g whiteConcerns
rateWhite (Left w) _
  | KingCapture <- w = -100000000
  | NoMoves     <- w =  100000000
  | NoPieces    <- w =  100000000
  | Escape      <- w =  100000000
  | _           <- w =  0

bestMove :: (Either WinLose Moves -> GameState -> Int) -> PostTurn -> PostTurn
bestMove _ (Left m,g)  = (Left m, g)
bestMove r (Right m,g) = maximumBy (comparing (uncurry r)) $ allGameStates g m

generateMove :: GameState -> Moves -> PostTurn
generateMove g = pickStrategy g g
