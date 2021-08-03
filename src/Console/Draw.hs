module Console.Draw
  ( drawUI
  , attributes
  ) where

import Console.AppState
import GameState
import Console.MovesAdapter (Tile(..), labelBoardMoves, labelBoardPieces, viewBoard)

import Brick
  ( Widget
  , AttrMap
  , hBox
  , vBox
  , AttrMap
  , attrMap
  , str
  , fill
  , (<=>)
  , (<+>)
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
  , borderWithLabel
  , hBorderWithLabel
  )
import Brick.Util (fg)
import Control.Lens ((&), (^.))
import Data.List (intersperse)
import Graphics.Vty (brightBlack, defAttr)

-------------------------------------------------------------------------------


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


attributes :: AttrMap
attributes =
  attrMap
    defAttr
    [ (borderAttr, fg brightBlack)
    ]

drawUI :: AppState -> [Widget Name]
drawUI a = [boardWidget a <=> logWidget a]

logWidget :: AppState -> Widget Name
logWidget a = (hBorderWithLabel $ str "Logs") <=> (vBox $ map str $ _logMessages a)

boardWidget :: AppState -> Widget Name
boardWidget a = case a ^. phase of
  ChoosePiece m -> a ^. gameState . board & (labelBoardPieces m) & drawBoard
  ChooseMove _ m -> a ^. gameState . board & (labelBoardMoves m) & drawBoard
  View m -> a ^. gameState . board & viewBoard & drawBoard
  _ -> str "Nothing to see here..."
