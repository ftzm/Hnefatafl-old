module Console.Events
  ( handleEvent
  ) where

import           Brick
  ( Next
  , BrickEvent (VtyEvent)
  , EventM
  , halt
  , continue
  )
import           Graphics.Vty
  ( Event (EvResize, EvKey)
  , Modifier (MCtrl)
  , Key (KChar)
  )

import           Console.AppState

-------------------------------------------------------------------------------

-- |Top-level event router
handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvResize x y))
  | x < 40 = halt s
handleEvent s (VtyEvent (EvKey (KChar c) [MCtrl]))
  | 'c' <- c = halt s
handleEvent s b = continue s
