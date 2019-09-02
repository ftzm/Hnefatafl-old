module Console.Draw
  ( drawUI
  , attributes
  ) where

import Console.AppState
import GameState
import Board (exportBoard, xyToInt, Coord)
import           Moves (SimpleMoves, exportMoves)

import Brick
  ( Widget
  , AttrMap
  , hBox
  , vBox
  , AttrMap
  , attrMap
  , str
  )
import Brick.Widgets.Core
  ( vLimit
  , hLimit
  , joinBorders
  )
import Brick.Widgets.Border
  ( border
  , vBorder
  , hBorder
  , borderAttr
  )
import           Data.List (foldl')
import qualified Data.Map.Strict as M (keys, fromList, lookup)
import           Data.Maybe (fromMaybe)

import Brick.Util (fg)
import Control.Lens ((&), (^.))
import Data.List (intersperse)
import Graphics.Vty (brightBlack, defAttr)

-------------------------------------------------------------------------------

data Tile = Symbol Char | Label Char

divBoard :: [a] -> [[a]]
divBoard [] = []
divBoard xs = splitAt 11 xs & \(x, y) -> x : divBoard y

drawBoard :: [Tile] -> Widget ()
drawBoard
  = vLimit 23     -- limit widget height
  . hLimit 45     -- limit widget width
  . joinBorders   -- Join the border glyphs where they meet
  . border        -- Put a border around the whole board
  . mkRows        -- transform list of rows widgets into single board widget
  . (map mkRow)   -- transform each row-list into a row widget
  . divBoard      -- turn list of tiles into list of row-lists
 where
  drawTile t = case t of
    Symbol c -> str [' ', c, ' ']
    Label c  -> str ['<', c, '>']
  mkRow  = hBox . intersperse vBorder . map drawTile
  mkRows = vBox . intersperse hBorder

labelledBoard :: [Coord] -> [Char] -> [Tile]
labelledBoard ms b =
  foldl'
    (\acc (c, i) -> acc ++ [fromMaybe (Symbol c) (Label <$> M.lookup i labelMap)])
    []
    $ zip b [0..]
  where labelMap = M.fromList $ zip (map xyToInt ms) ['a'..]

attributes :: AttrMap
attributes =
  attrMap
    defAttr
    [ (borderAttr, fg brightBlack)
    ]

drawUI :: AppState -> [Widget Name]
drawUI a = case a ^. phase of
  ChoosePiece m -> [a ^. gameState . board & exportBoard & (labelledBoard $ M.keys m) & drawBoard]
  ChooseMove m -> [a ^. gameState . board & exportBoard & (labelledBoard m) & drawBoard]
