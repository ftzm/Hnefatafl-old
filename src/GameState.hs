{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Control.Lens

import Board
import Moves

data GameState = GameState
    { _board :: Board
    , _king :: Coord
    , _whiteIsHuman :: Bool
    , _blackIsHuman :: Bool
    , _whiteTurn :: Bool
    , _lastMove :: (Square,Square)
    , _whiteLosses :: Int
    , _blackLosses :: Int
    , _allMoves :: AllMoves
    }
  deriving (Show)

makeLenses ''GameState
