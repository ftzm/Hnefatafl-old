{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Console.Events
  ( handleEvent
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import           Brick
  ( Next
  , BrickEvent (..)
  , EventM
  , halt
  , continue
  )
import           Brick.BChan (newBChan, readBChan, writeBChan, writeBChanNonBlocking, BChan)
import           Graphics.Vty
  ( Event (EvResize, EvKey)
  , Modifier (MCtrl)
  , Key (KChar)
  )
import Control.Lens ((^.), (.~), (%~), (&))
import           Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Async

import           Console.AppState
import           Moves (SimpleMoves, exportMoves)
import           Console.MovesAdapter (charToPieceMoves, charToPieceMove)
import Board (Coord)
import Turns (runTurn)
import Capability
import Text.Printf

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
handleEvent s (AppEvent (SelectionStatus msg)) = continue $ s & selectionStatus .~ msg
handleEvent s e@(Keys _ _) = (liftIO $ writeBChan (_chan s) $ Log $ "Handled event during phase: " ++ (s ^. phase & show)) >> case s ^. phase of
  View m -> (continue $ viewEvent s e m)
  ChoosePiece m -> continue $ choosePieceEvent s e m
  ChooseMove p m -> do
    s' <- liftIO  $ chooseMoveEvent s e p m
    continue s'
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

chooseMoveEvent :: AppState -> MyAppEvent -> Coord -> [Coord] -> IO AppState
chooseMoveEvent s (Keys c []) origin m =
  case charToPieceMove c m of
    Just destination -> case runTurn (s ^. gameState) (origin, destination) of
        (Left wl, gs) ->
          return $ s & gameState .~ gs & phase .~ GameOver (show wl)
        (Right moves, gs) -> do
          traverse cancel $ s ^. humanCounter
          pendingEvent <- readBChan $ s ^. chan
          handle <- async (runHumanCounter (s ^. chan))
          let s' = s & humanCounter .~ Just handle
          return $ s' & gameState .~ gs & phase .~ View ( exportMoves moves)
    Nothing -> return s
chooseMoveEvent s _ _ _ = return s

runHumanCounter :: BChan AsyncEvent -> IO ()
runHumanCounter chan =
  mapM_ send [0..]
  where
    format :: Integer -> String
    format i =
      let (minutes, seconds) = divMod i 60
      in show minutes ++ ":" ++ printf "%02d" seconds
    send i = do
      writeBChanNonBlocking chan $ SelectionStatus $ format i
      threadDelay 1000000 -- decides how frequently the Tick is sent

data Spinner = SpinnerOne | SpinnerTwo | SpinnerThree

nextSpin SpinnerOne = SpinnerTwo
nextSpin SpinnerTwo = SpinnerThree
nextSpin SpinnerThree = SpinnerOne
