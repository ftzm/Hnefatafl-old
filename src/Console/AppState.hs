{-# LANGUAGE TemplateHaskell #-}

module Console.AppState
  ( Name
  , AsyncEvent (..)
  , Phase (..)
  , AppState (..)
  , GameOptions (..)
  , PlayerType (..)
  , gameState
  , phase
  , logMessages
  , startState
  , gameOptions
  , blackPlayer
  , whitePlayer
  , chan
  , selectionStatus
  , humanCounter
  ) where

import           GameState (GameState)
import           Moves (SimpleMoves, startMovesBlack, exportMoves)
import           Board (Coord)
import           Engine (startGame)
import           AI.Dispatch

import           Brick.BChan (BChan)

import           Control.Lens (makeLenses)
import Control.Concurrent.Async

-------------------------------------------------------------------------------

data PlayerType = Human | AI AIType
  deriving Show

data GameOptions = GameOptions
  { _blackPlayer :: PlayerType
  , _whitePlayer :: PlayerType
  , _recordGame :: Bool
  } deriving Show
makeLenses ''GameOptions

type Name = ()

data AsyncEvent
  = SelectionStatus String
  | Log String

data Phase
  = View SimpleMoves
  | ChoosePiece SimpleMoves
  | ChooseMove Coord [Coord]
  | GameOver String
  | Wait
  | End
  deriving (Show)

data AppState = AppState
  { _gameState :: GameState
  , _gameOptions :: GameOptions
  , _phase :: Phase
  , _logMessages :: [String]
  , _chan :: BChan AsyncEvent
  , _selectionStatus :: String
  , _humanCounter :: Maybe (Async ())
  }
makeLenses ''AppState

startState :: GameOptions -> BChan AsyncEvent -> AppState
startState o c = AppState startGame o (ChoosePiece $ exportMoves startMovesBlack) ["The game is afoot!"] c "" Nothing

-------------------------------------------------------------------------------
-- Options
