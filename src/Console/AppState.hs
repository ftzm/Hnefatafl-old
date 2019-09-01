{-# LANGUAGE TemplateHaskell #-}

module Console.AppState
  ( Name
  , Tick (..)
  , Phase (..)
  , AppState (..)
  , gameState
  , startState
  , GameOptions (..)
  , PlayerType (..)
  ) where

import           GameState (GameState)
import           Engine (startGame)
import           Control.Lens (makeLenses)

-------------------------------------------------------------------------------

type Name = ()

data Tick = Tick

data Phase
  = View
  | ChooseMove
  | Wait
  | End
  deriving (Show)

data AppState = AppState
  { _gameState :: GameState
  , _phase :: Phase
  }
  deriving (Show)
makeLenses ''AppState

startState :: AppState
startState = AppState startGame Wait

-------------------------------------------------------------------------------
-- Options

data PlayerType = Human | AI String
  deriving Show

data GameOptions = GameOptions
  { blackPlayer :: PlayerType
  , whitePlayer :: PlayerType
  , recordGame :: Bool
  } deriving Show
