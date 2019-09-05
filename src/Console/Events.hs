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
import Control.Lens ((^.), (.~), (&))
import Data.List (elemIndex, (!!))
import Data.Maybe (maybe, fromJust)

import           Console.AppState
import           Moves (SimpleMoves)
import           Console.MovesAdapter (charToPieceMoves)
import Board (Coord)

-------------------------------------------------------------------------------

-- |Top-level event router
handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvResize x y))
  | x < 40 = halt s
handleEvent s (VtyEvent (EvKey (KChar c) [MCtrl]))
  | 'c' <- c = halt s
handleEvent s e = case s ^. phase of
  View -> continue s
  ChoosePiece m -> continue $ choosePieceEvent s e m
  ChooseMove p m -> continue $ chooseMoveEvent s e m
  Wait -> continue s
  End -> continue s

choosePieceEvent :: AppState -> BrickEvent Name Tick -> SimpleMoves -> AppState
choosePieceEvent s (VtyEvent (EvKey (KChar c) [])) m =
  case charToPieceMoves c m of
    Just (piece, moves) -> s & phase .~ ChooseMove piece moves
    Nothing -> s
choosePieceEvent s e m = s

chooseMoveEvent :: AppState -> BrickEvent Name Tick -> [Coord] -> AppState
chooseMoveEvent s (VtyEvent (EvKey (KChar c) [])) m = s
chooseMoveEvent s e m = s
