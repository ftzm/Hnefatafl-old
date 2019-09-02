{-# LANGUAGE TemplateHaskell #-}

module Console.AppState
  ( Name
  , Tick (..)
  , Phase (..)
  , AppState (..)
  , GameOptions (..)
  , PlayerType (..)
  , gameState
  , phase
  , startState
  ) where

import           GameState (GameState)
import           Moves (SimpleMoves, startMovesBlack, exportMoves)
import           Board (Coord, Direction)
import           Engine (startGame)

import           Control.Lens (makeLenses)
import qualified Data.Map.Strict as M

-------------------------------------------------------------------------------

type Name = ()

data Tick = Tick

data Phase
  = View
  | ChoosePiece SimpleMoves
  | ChooseMove [Coord]
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
startState = AppState startGame $ ChoosePiece $ exportMoves startMovesBlack

-------------------------------------------------------------------------------
-- Options

data PlayerType = Human | AI String
  deriving Show

data GameOptions = GameOptions
  { blackPlayer :: PlayerType
  , whitePlayer :: PlayerType
  , recordGame :: Bool
  } deriving Show
