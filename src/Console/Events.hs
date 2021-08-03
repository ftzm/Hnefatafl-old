{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Console.Events
  ( handleEvent
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import           Brick
  ( Next
  , BrickEvent (..)
  , EventM
  , halt
  , continue
  )
import           Brick.BChan (newBChan, writeBChan, BChan)
import           Graphics.Vty
  ( Event (EvResize, EvKey)
  , Modifier (MCtrl)
  , Key (KChar)
  )
import Control.Lens ((^.), (.~), (%~), (&))

import           Console.AppState
import           Moves (SimpleMoves, exportMoves)
import           Console.MovesAdapter (charToPieceMoves, charToPieceMove)
import Board (Coord)
import Turns (runTurn)
import Capability

-------------------------------------------------------------------------------

data ConsoleRunnerR = ConsoleRunnerR { _bchan :: BChan AsyncEvent }

newtype ConsoleRunnerT m a
  = ConsoleRunnerT { unConsoleRunnterT :: ReaderT ConsoleRunnerR m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

runConsoleRunnerT :: MonadIO m => ConsoleRunnerR -> ConsoleRunnerT m a -> m a
runConsoleRunnerT r (ConsoleRunnerT m) = runReaderT m r

instance MonadIO m => MonadLog (ConsoleRunnerT m) where
  logDebug msg = ConsoleRunnerT $ do
    chan <- _bchan <$> ask
    liftIO $ writeBChan chan $ Log "test"


pattern Keys c ms = (VtyEvent (EvKey (KChar c) ms))
pattern Resize x y = (VtyEvent (EvResize x y))
type MyAppEvent = BrickEvent Name AsyncEvent
type AppNext = EventM Name (Next AppState)

-- |Top-level event router
handleEvent :: AppState -> MyAppEvent -> AppNext
handleEvent s (Resize x y)
  | x < 40 = halt s
  | y < 40 = halt s
  | otherwise = continue s
handleEvent s (Keys 'c' [MCtrl]) = halt s
handleEvent s (AppEvent (Log msg)) = continue $ s & logMessages %~ \xs -> msg:xs
--handleEvent s (AppEvent Tick) = continue $ s & logMessages %~ \xs -> "tick":xs
handleEvent s e@(Keys _ _) = (liftIO $ writeBChan (_chan s) $ Log $ "Handled event during phase: " ++ (s ^. phase & show)) >> case s ^. phase of
  View m -> (continue $ viewEvent s e m)
  ChoosePiece m -> continue $ choosePieceEvent s e m
  ChooseMove p m -> continue $ chooseMoveEvent s e p m
  Wait -> continue s
  GameOver _ -> continue s
  End -> continue s
handleEvent s _ = continue s

viewEvent :: AppState -> MyAppEvent -> SimpleMoves -> AppState
viewEvent s (Keys c []) m = s & phase .~ ChoosePiece m
viewEvent s _ _ = s

choosePieceEvent :: AppState -> MyAppEvent -> SimpleMoves -> AppState
choosePieceEvent s (Keys c []) m =
  case charToPieceMoves c m of
    Just (piece, moves) -> s & phase .~ ChooseMove piece moves
    Nothing -> s
choosePieceEvent s _ _ = s

chooseMoveEvent :: AppState -> MyAppEvent -> Coord -> [Coord] -> AppState
chooseMoveEvent s (Keys c []) origin m =
  case charToPieceMove c m of
    Just destination -> case runTurn (s ^. gameState) (origin, destination) of
        (Left wl, gs) ->
          s & gameState .~ gs & phase .~ GameOver (show wl)
        (Right moves, gs) ->
          s & gameState .~ gs & phase .~ View ( exportMoves moves)
    Nothing -> s
chooseMoveEvent s _ _ _ =  s
