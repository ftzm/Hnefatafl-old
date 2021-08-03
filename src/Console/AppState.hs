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
  ) where

import           GameState (GameState)
import           Moves (SimpleMoves, startMovesBlack, exportMoves)
import           Board (Coord)
import           Engine (startGame)
import           Brick.BChan (BChan)

import           Control.Lens (makeLenses)

-------------------------------------------------------------------------------

type Name = ()

data AsyncEvent
  = Tick
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
  , _phase :: Phase
  , _logMessages :: [String]
  , _chan :: BChan AsyncEvent
  }
makeLenses ''AppState

startState :: BChan AsyncEvent -> AppState
startState = AppState startGame (ChoosePiece $ exportMoves startMovesBlack) ["The game is afoot!"]

-------------------------------------------------------------------------------
-- Options

data PlayerType = Human | AI String
  deriving Show

data GameOptions = GameOptions
  { blackPlayer :: PlayerType
  , whitePlayer :: PlayerType
  , recordGame :: Bool
  } deriving Show
