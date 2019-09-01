module Console.Draw
  ( drawUI
  , attributes
  ) where

import Console.AppState
import GameState
import Board (exportBoard)

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
import Brick.Util (fg)
import Control.Lens ((&), (^.))
import Data.List (intersperse)
import Graphics.Vty (brightBlack, defAttr)

-------------------------------------------------------------------------------

data Tile = Piece | Label Char

divBoard :: [a] -> [[a]]
divBoard [] = []
divBoard xs = splitAt 11 xs & \(x, y) -> x : divBoard y

drawBoard :: [Char] -> Widget ()
drawBoard
  = vLimit 23     -- limit widget height
  . hLimit 45     -- limit widget width
  . joinBorders   -- Join the border glyphs where they meet
  . border        -- Put a border around the whole board
  . mkRows        -- transform list of rows widgets into single board widget
  . (map mkRow)   -- transform each row-list into a row widget
  . divBoard      -- turn list of tiles into list of row-lists
 where
  mkRow  = hBox . intersperse vBorder . map (\s -> str [' ', s, ' '])
  mkRows = vBox . intersperse hBorder

attributes :: AttrMap
attributes =
  attrMap
    defAttr
    [ (borderAttr, fg brightBlack)
    ]

drawUI :: AppState -> [Widget Name]
drawUI a = [a ^. gameState . board & exportBoard & drawBoard]
