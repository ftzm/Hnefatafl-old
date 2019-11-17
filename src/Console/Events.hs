{-# LANGUAGE PatternSynonyms  #-}

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

import           Console.AppState
import           Moves (SimpleMoves, exportMoves)
import           Console.MovesAdapter (charToPieceMoves, charToPieceMove)
import Board (Coord)
import Turns (runTurn)

-------------------------------------------------------------------------------

pattern Keys c ms = (VtyEvent (EvKey (KChar c) ms))
pattern Resize x y = (VtyEvent (EvResize x y))
type AppEvent = BrickEvent Name Tick
type AppNext = EventM Name (Next AppState)

-- |Top-level event router
handleEvent :: AppState -> AppEvent -> AppNext
handleEvent s (Resize x y)
  | x < 40 = halt s
  | y < 40 = halt s
  | otherwise = continue s
handleEvent s (Keys 'c' [MCtrl]) = halt s
handleEvent s e = case s ^. phase of
  View -> continue s
  ChoosePiece m -> continue $ choosePieceEvent s e m
  ChooseMove p m -> continue $ chooseMoveEvent s e p m
  Wait -> continue s
  GameOver _ -> continue s
  End -> continue s

choosePieceEvent :: AppState -> AppEvent -> SimpleMoves -> AppState
choosePieceEvent s (Keys c []) m =
  case charToPieceMoves c m of
    Just (piece, moves) -> s & phase .~ ChooseMove piece moves
    Nothing -> s
choosePieceEvent s _ _ = s

chooseMoveEvent :: AppState -> AppEvent -> Coord -> [Coord] -> AppState
chooseMoveEvent s (Keys c []) origin m =
  case charToPieceMove c m of
    Just destination -> case runTurn (s ^. gameState) (origin, destination) of
        (Left wl, gs) ->
          s & gameState .~ gs & phase .~ GameOver (show wl)
        (Right moves, gs) ->
          s & gameState .~ gs & phase .~ ChoosePiece ( exportMoves moves)
    Nothing -> s
chooseMoveEvent s _ _ _ = s
