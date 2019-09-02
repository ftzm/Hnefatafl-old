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
import qualified Data.Map.Strict as M (lookup, keys, elems, fromList)
import Data.List (elemIndex, (!!))
import Data.Maybe (maybe, fromJust)

import           Console.AppState
import           Moves (SimpleMoves)
import           Board (intToXY, Coord)

-------------------------------------------------------------------------------

-- |Top-level event router
handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvResize x y))
  | x < 40 = halt s
handleEvent s (VtyEvent (EvKey (KChar c) [MCtrl]))
  | 'c' <- c = halt s
handleEvent s e = case s ^. phase of
  View -> continue s
  ChoosePiece m -> choosePieceEvent s e m
  ChooseMove m -> continue s
  Wait -> continue s
  End -> continue s

choosePieceEvent :: AppState -> BrickEvent Name Tick -> SimpleMoves -> EventM Name (Next AppState)
choosePieceEvent s (VtyEvent (EvKey (KChar c) [])) m =
  maybe (continue s) (\x -> continue $ s & phase .~ (ChooseMove x)) (M.lookup c movesMap)
  where movesMap = M.fromList $ zip (map fst (zip ['a'..] (M.keys m))) (M.elems m)
choosePieceEvent s e m = continue s

chooseMoveEvent :: AppState -> BrickEvent Name Tick -> [Coord] -> EventM Name (Next AppState)
chooseMoveEvent s (VtyEvent (EvKey (KChar c) [])) m = continue s
chooseMoveEvent s e m = continue s
