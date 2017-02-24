{-# LANGUAGE TupleSections  #-}

module Moves
  (updateMovesAdd
  ,updateMovesDelete
  ,opp
  ,findNextPiece
  ,dirMoves
  ,allMovesSplit
  ,updateMoves
  ,absenceUpdate
  ,updatePerpOrigin
  )
where

import qualified Data.Map.Strict as M
import Data.List
import Control.Applicative
import Control.Lens hiding (Empty)

import Board
import GameState

opp :: Direction -> Direction
opp North = South
opp South = North
opp East  = West
opp West  = East

perp :: Direction -> [Direction]
perp North = [East,West]
perp East = [North,South]
perp South = [East,West]
perp West = [North,South]

dirMoves :: Board -> Square -> Direction -> [Coord]
dirMoves b s@(_,p) = takeWhile (eligible . snd . getSquare b) . toEdge s
  where eligible x | p == King = x == Empty || x == Corner
                   | otherwise = x == Empty

pieceMovesSplit :: Board -> Square -> M.Map Direction [Coord]
pieceMovesSplit b s = foldl' (\acc x -> acc & at x ?~ dirMoves b s x) M.empty [North,East,South,West]

findNextPiece :: Board -> Square -> Direction -> Maybe Square
findNextPiece b s d = case go b s d of
                         Just s'@(_,Empty) -> findNextPiece b s' d
                         Just (_,Corner) -> Nothing
                         Just s' -> Just s'
                         Nothing -> Nothing

modifyWhiteMoves :: GameState -> (Moves -> Moves) -> GameState
modifyWhiteMoves g f = g {whiteMoves = f $ whiteMoves g}

modifyBlackMoves :: GameState -> (Moves -> Moves) -> GameState
modifyBlackMoves g f = g {blackMoves = f $ blackMoves g}

modifyMoves :: Square -> (GameState -> (Moves -> Moves) -> GameState)
modifyMoves (_,p) | p == Black = modifyBlackMoves | otherwise  = modifyWhiteMoves

modifyDirMoves :: Coord -> Direction -> ([Coord] -> [Coord]) -> Moves -> Moves
modifyDirMoves c d f = at c . non M.empty . at d %~ (f <$>)

writeDirMoves :: Square -> Direction -> [Coord] -> Moves -> Moves
writeDirMoves (c,_) d cs = at c . non M.empty . at d ?~ cs

adjustDirMoves :: Square -> GameState -> Direction -> GameState
adjustDirMoves s g d = maybe g (write g d) nextPiece
  where
    b = board g
    nextPiece = findNextPiece b s d
    write g' d' s'= modifyMoves s' g' (writeDirMoves s' (opp d') (dirMoves b s' (opp d')))

updateMovesAround :: GameState -> Square -> GameState

updateMovesAround g s = foldl' (adjustDirMoves s) g [North,East,South,West]

deleteMoves :: GameState -> Square -> GameState
deleteMoves g s@(c,_) = modifyMoves s g (M.delete c)

addMoves :: GameState -> Square -> M.Map Direction [Coord] -> GameState
addMoves g s@(c,_) ms = modifyMoves s g (M.insert c ms)

updateMovesDelete :: GameState -> Square -> GameState
updateMovesDelete g s = updateMovesAround (deleteMoves g s) s

updateMovesAdd :: GameState -> Square -> GameState
updateMovesAdd g s = updateMovesAround (addMoves g s (pieceMovesSplit (board g) s)) s

allMovesSplit :: GameState -> Moves
allMovesSplit g = foldl' buildMap M.empty squares
  where
    buildMap acc s@(x,_) = M.insert x (pieceMovesSplit (board g) s) acc
    squares = if whiteTurn g
                 then zip whiteStart (repeat White)
                 else zip blackStart (repeat Black)

moveDir :: (Coord,Coord) -> Direction
moveDir ((x1,y1),(x2,y2))
  | x1 > x2 = West
  | x1 < x2 = East
  | y1 > y2 = North
  | otherwise = South

getDirDestinations :: GameState -> Square -> Direction -> [Coord]
getDirDestinations g (c,p) d = ms ^. at c . _Just ^. at d . non []

  where
    ms = if whitePiece p then whiteMoves g else blackMoves g

-- ---------------------------------------------------------------------------
-- Functions for updating the map of moves.

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

beyondLastDestination :: Board -> Square -> [Coord] -> Direction -> Maybe Square
beyondLastDestination b s cs d =
  case flip (go b) d =<< lastSquare of
    Just s@(c,Black) -> Just s
    Just s@(c,White) -> Just s
    Just s@(c,King)  -> Just s
    _              -> Nothing
  where
    lastSquare = ((,Empty) <$> safeLast cs) <|> Just s

-- |Given the coordinates where a piece was, update the moves of the nearest
-- piece in a perpendicular direction.
--absenceUpdate
--  :: Square    -- ^ Origin Square
--  -> Direction -- ^ Direction to look for a piece whose moves to update
absenceUpdate s g d = maybe g f target
  where
    b = board g
    [targetSide,otherSide] = map (getDirDestinations g s) [d, opp d]
    f s'@(c,p) = modifyMoves s' g $ modifyDirMoves c (opp d) (\xs -> xs ++ [fst s]++ otherSide)
    target = beyondLastDestination b s targetSide d

-- |Takes a map of moves where a piece is going to be moved and updates the
-- moves of pieces perpendicular to the move to account for its absence at the
-- origin.
-- Requires a moves map which has not yet had the origin piece deleted
updatePerpOrigin :: Square -> Direction -> GameState -> GameState
updatePerpOrigin s d g = foldl' (absenceUpdate s) g $ perp d

trailToNext :: Board -> Piece -> Square ->  [Coord] -> Direction -> ([Coord] , Maybe Square)
trailToNext b startP s cs d
  | Just s'@(c,p) <- next
  = case p of
      Empty -> trailToNext b startP s' (c:cs) d
      Corner -> if startP == King
                then (c:cs, Nothing)
                else (cs, Nothing)
      p'     -> if isMobile p'
                then (cs, Just s')
                else (cs, Nothing)
  | Nothing <- next = (cs, Nothing)
  where
    next = go b s d
    isMobile p = p == Black || p == White || p == King

updateByBoard :: Square -> GameState -> Direction -> GameState
updateByBoard s g d
  = flip (f d (reverse trail)) s
  $ maybe g (f (opp d) trail g) next
  where
    (trail, next) = trailToNext (board g) (snd s) s [] d
    f d t g s'@(c,p) = modifyMoves s' g $ modifyDirMoves c d (const t)

updatePerpDestination :: Square -> Direction -> GameState -> GameState
updatePerpDestination s d g
  = foldl' (updateByBoard s) g $ perp d

updateAlongMove :: Square -> Direction -> GameState -> GameState
updateAlongMove s d g
  = foldl' (updateByBoard s) g [d, opp d]

-- |Function to get axis of a move. temporary until movement dir is baked in
movementAxis :: Square -> Square -> Direction
movementAxis ((x1,y1),_) ((x2,y2),_)
  | x1 == x2 = North
  | y1 == y2 = West
  | otherwise = North

updateMoves :: GameState -> GameState
updateMoves g
  = updateAlongMove dest mDir
  $ updatePerpDestination dest mDir
  $ (\g' -> addMoves g' dest (M.fromList $ map (,[]) [North,East,South,West]))
  $ flip deleteMoves origin
  $ updatePerpOrigin origin mDir g
  where
    lm = lastMove g
    mDir = uncurry movementAxis lm
    origin = fst lm
    dest = snd lm
