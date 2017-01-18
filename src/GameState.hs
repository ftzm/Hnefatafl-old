{-# LANGUAGE DeriveAnyClass  #-}

module GameState where

import qualified Data.Map.Strict as M

  --,GameState
  --  (board
  --  ,whiteIsHuman
  --  ,blackIsHuman
  --  ,whiteTurn
  --  ,lastMove
  --  ,whiteLosses
  --  ,blackLosses
  --  ,whiteMoves
  --  ,blackMoves
  --  )

import Board

type Moves = M.Map Coord [[Coord]]

data GameState = GameState
    { board :: Board
    , king :: Coord
    , whiteIsHuman :: Bool
    , blackIsHuman :: Bool
    , whiteTurn :: Bool
    , lastMove :: (Square,Square)
    , whiteLosses :: Int
    , blackLosses :: Int
    , whiteMoves :: Moves
    , blackMoves :: Moves
    }
  deriving (Show)
