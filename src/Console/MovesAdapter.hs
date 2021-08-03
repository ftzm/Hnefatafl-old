------------------------------------------------------------------------------
-- Functions for converting the board to representations that include labels
-- for selecting pieces and moves, and converting these labels into coordinates
-- to feed into board update functions.
------------------------------------------------------------------------------

module Console.MovesAdapter
  ( Tile(..)
  , charToPieceMoves
  , charToPieceMove
  , labelBoardPieces
  , labelBoardMoves
  , viewBoard
  ) where

import           Prelude hiding (lookup)
import           Board (exportBoard, xyToInt, Coord, Board)
import           Moves (SimpleMoves)

import           Data.Map.Strict (keys, fromList, lookup, toAscList)
import           Data.Maybe (maybe)

-------------------------------------------------------------------------------
-- Select

charToPieceMoves :: Char -> SimpleMoves -> Maybe (Coord, [Coord])
charToPieceMoves c = lookup c . fromList . zip ['a'..] . toAscList

charToPieceMove :: Char -> [Coord] -> Maybe Coord
charToPieceMove c = lookup c . fromList . zip ['a'..]

-------------------------------------------------------------------------------
-- Display

data Tile = Symbol Char | Label Char

labelledBoard :: [Coord] -> [Char] -> [Tile]
labelledBoard ms = map mkTile . zip [0..]
  where
    labelMap = fromList $ zip (map xyToInt ms) ['a'..]
    mkTile (i, c) = maybe (Symbol c) Label (lookup i labelMap)

viewBoard :: Board -> [Tile]
viewBoard b = (map Symbol (exportBoard b))

labelBoardPieces :: SimpleMoves -> Board -> [Tile]
labelBoardPieces m b = labelledBoard (keys m) (exportBoard b)

labelBoardMoves :: [Coord] -> Board -> [Tile]
labelBoardMoves cs = labelledBoard cs . exportBoard
