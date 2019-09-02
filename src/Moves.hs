{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Moves
  (findNextPiece
  ,dirMoves
  ,postMoveUpdate
  ,postDeleteUpdate
  ,startMovesWhite
  ,startMovesBlack
  ,startMoves
  ,Moves
  ,SimpleMoves
  ,AllMoves
  ,blackMoves
  ,whiteMoves
  ,exportMoves
  )
where

import           Board
import           Control.Lens    hiding (Empty)
import           Data.List
import qualified Data.Map.Strict as M

type Moves = M.Map Coord (M.Map Direction [Coord])
type SimpleMoves = M.Map Coord [Coord]

data AllMoves = AllMoves
  { _whiteMoves :: Moves
  , _blackMoves :: Moves
  } deriving (Show)

makeLenses ''AllMoves

dirs :: [Direction]
dirs = [North,East,South,West]

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
pieceMovesSplit b s = foldl' (\a x -> a & at x ?~ dirMoves b s x) M.empty dirs

findNextPiece :: Board -> Square -> Direction -> Maybe Square
findNextPiece b s d = case go b s d of
                         Just s'@(_,Empty) -> findNextPiece b s' d
                         Just (_,Corner) -> Nothing
                         Just s' -> Just s'
                         Nothing -> Nothing

modifyMoves :: Square -> AllMoves -> (Moves -> Moves) -> AllMoves
modifyMoves (_,p) m f = m & k %~ f
  where k | p == Black = blackMoves | otherwise = whiteMoves

writeDirMoves :: Square -> Direction -> [Coord] -> Moves -> Moves
writeDirMoves (c,_) d cs = at c . _Just . at d ?~ cs

adjustDirMoves :: Square -> Board -> AllMoves -> Direction -> AllMoves
adjustDirMoves s b m d = maybe m (write m d) $ findNextPiece b s d
  where
    write m' d' s'= modifyMoves s' m'
      (writeDirMoves s' (opp d') (dirMoves b s' (opp d')))

updateMovesAround :: AllMoves -> Board -> Square -> [Direction] -> AllMoves
updateMovesAround m b s = foldl' (adjustDirMoves s b) m

deleteMoves :: AllMoves -> Square -> AllMoves
deleteMoves m s@(c,_) = modifyMoves s m (M.delete c)

addMoves :: AllMoves -> Square -> M.Map Direction [Coord] -> AllMoves
addMoves m s@(c,_) ms = modifyMoves s m (M.insert c ms)

updateMovesDelete :: AllMoves -> Board -> Square -> [Direction] -> AllMoves
updateMovesDelete m b s = updateMovesAround (deleteMoves m s) b s

updateMovesAdd :: Board -> AllMoves -> Square -> AllMoves
updateMovesAdd b m s
  = updateMovesAround (addMoves m s (pieceMovesSplit b s)) b s dirs

moveAxis :: Square -> Square -> Direction
moveAxis ((x1,_),_) ((x2,_),_)
  | x1 == x2 = North
  | otherwise = East

postMoveUpdate :: (Square,Square) -> Board -> AllMoves -> AllMoves
postMoveUpdate (s1,s2) b m = m''
  where
    mAx = moveAxis s1 s2
    m' = updateMovesDelete m b s1 (perp mAx)
    m'' = updateMovesAdd b m' s2

postDeleteUpdate :: Board -> AllMoves -> [Square] -> AllMoves
postDeleteUpdate b = foldl' (\m' x -> updateMovesDelete m' b x dirs)

allMovesSplit :: [Square] -> Moves
allMovesSplit = foldl' buildMap M.empty
  where
    buildMap acc s@(x,_) = M.insert x (pieceMovesSplit startBoard s) acc

startMovesWhite :: Moves
startMovesWhite = allMovesSplit $ zip (throne:whiteStart) (repeat White)

startMovesBlack :: Moves
startMovesBlack = allMovesSplit $ zip blackStart (repeat Black)

startMoves :: AllMoves
startMoves = AllMoves { _whiteMoves = startMovesWhite , _blackMoves = startMovesBlack }

exportMoves :: Moves -> SimpleMoves
exportMoves = M.filter (not . null) . M.map (concat . M.elems)
