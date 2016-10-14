{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass  #-}

module GameState where

import qualified Data.ByteString.Lazy.Char8 as BS (toStrict)
import qualified Data.ByteString.Internal as BI (ByteString)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Map.Strict as M
import GHC.Generics

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
    , whiteIsHuman :: Bool
    , blackIsHuman :: Bool
    , whiteTurn :: Bool
    , lastMove :: (Square,Square)
    , whiteLosses :: Int
    , blackLosses :: Int
    , whiteMoves :: Moves
    , blackMoves :: Moves
    }
  deriving (Show,Generic,ToJSON,FromJSON)

gameToJSON :: GameState -> BI.ByteString
gameToJSON x = BS.toStrict $ encodePretty (x :: GameState)
