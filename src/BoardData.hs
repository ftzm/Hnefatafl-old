module BoardData where

import qualified Data.IntMap.Strict as IM

data Piece = White | Black | King | Empty | Corner
  deriving (Show, Eq)
data Direction = North | South | East | West
  deriving (Show, Eq)
type Coord = (Int,Int)
type Square = (Coord,Piece)
type Board = IM.IntMap Piece

boardSize :: Int
boardSize = 11

cornerCoords :: [Coord]
cornerCoords = [(0,0),(10,0),(0,10),(10,10)]

blackStart :: [Coord]
blackStart = [(3,0),(4,0),(5,0),(6,0),(7,0),(5,1),(0,3),(10,3),(0,4),(10,4),
              (0,5),(1,5),(9,5),(10,5),(0,6),(10,6),(0,7),(10,7),(5,9),(3,10),
              (4,10),(5,10),(6,10),(7,10)]

whiteStart :: [Coord]
whiteStart = [(5,3),(4,4),(5,4),(6,4),(3,5),(4,5),(6,5),(7,5),(4,6),(5,6),
              (6,6),(5,7)]

kingStart :: Coord
kingStart = (5,5)
