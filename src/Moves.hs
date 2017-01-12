module Moves
  (updateMovesAdd
  ,updateMovesDelete
  ,opp
  ,findNextPiece
  ,dirMoves
  ,allMovesSplit
  )
where

import qualified Data.Map.Strict as M
import Data.List
import qualified Data.IntSet as S

import Board
import GameState

opp :: Direction -> Direction
opp North = South
opp South = North
opp East  = West
opp West  = East

dirMoves :: Board -> Square -> Direction -> [Coord]
dirMoves b s@(_,p) = takeWhile (eligible . snd . getSquare b) . toEdge s
  where eligible x | p == King = x == Empty || x == Corner
                   | otherwise = x == Empty

pieceMovesSplit :: Board -> Square -> [[Coord]]
pieceMovesSplit b s = map (dirMoves b s) [North,East,South,West]

findNextPiece :: Board -> Square -> Direction -> Maybe Square
findNextPiece b s d = find (\(_,p) -> (p /= Empty && p /= Corner)) $ map (getSquare b) $ toEdge s d

modifyWhiteMoves :: GameState -> (Moves -> Moves) -> GameState
modifyWhiteMoves g f = g {whiteMoves = f $ whiteMoves g}

modifyBlackMoves :: GameState -> (Moves -> Moves) -> GameState
modifyBlackMoves g f = g {blackMoves = f $ blackMoves g}

modifyMoves :: Square -> (GameState -> (Moves -> Moves) -> GameState)
modifyMoves (_,p) | p == Black = modifyBlackMoves | otherwise  = modifyWhiteMoves

writeDirMoves :: Square -> Direction -> [Coord] -> Moves -> Moves
writeDirMoves (c,_) d cs m
  | Just existing <- M.lookup c m = M.insert c (f d existing cs) m
  | Nothing <- M.lookup c m = M.insert c (f d [[],[],[],[]] cs) m
  where f d' [h,i,j,k] x
          | North <- d' = [x,i,j,k]
          | East  <- d' = [h,x,j,k]
          | South <- d' = [h,i,x,k]
          | West  <- d' = [h,i,j,x]

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

addMoves :: GameState -> Square -> [[Coord]] -> GameState
addMoves g s@(c,_) ms = modifyMoves s g (M.insert c ms)

updateMovesDelete :: GameState -> Square -> GameState
updateMovesDelete g s = updateMovesAround (deleteMoves g s) s

updateMovesAdd :: GameState -> Square -> GameState
updateMovesAdd g s = updateMovesAround (addMoves g s (pieceMovesSplit (board g) s)) s

allMovesSplit :: GameState -> Moves
allMovesSplit g = foldl' buildMap M.empty squares
  where
    buildMap acc s@(x,_)
      | all null $ pieceMovesSplit (board g) s = acc
      | otherwise = M.insert x (pieceMovesSplit (board g) s) acc
    squares = if whiteTurn g
                 then zip (map intToXY $ S.toList $ whites $ board g) (repeat White)
                 else zip (map intToXY $ S.toList $ blacks $ board g) (repeat Black)
