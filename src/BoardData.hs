module BoardData where

import qualified Data.IntSet as S

data Piece = White | Black | King | Empty | Corner
  deriving (Show, Eq)
data Direction = North | South | East | West
  deriving (Show, Eq)
type Coord = Int
type Square = (Coord,Piece)
--type Board = IM.IntMap Piece

boardSize :: Int
boardSize = 11

xyToInt :: (Int,Int) -> Int
xyToInt (x,y) = boardSize * y + x

blackStart :: [Coord]
blackStart = map xyToInt [(3,0),(4,0),(5,0),(6,0),(7,0),(5,1),(0,3),(10,3),(0,4),(10,4),
              (0,5),(1,5),(9,5),(10,5),(0,6),(10,6),(0,7),(10,7),(5,9),(3,10),
              (4,10),(5,10),(6,10),(7,10)]

whiteStart :: [Coord]
whiteStart = map xyToInt [(5,3),(4,4),(5,4),(6,4),(3,5),(4,5),(6,5),(7,5),(4,6),(5,6),
              (6,6),(5,7)]

cornerCoords :: [Coord]
cornerCoords = map xyToInt [(0,0),(10,0),(0,10),(10,10)]

throne :: Coord
throne = xyToInt (5,5)

data Board = Board
    { blacks :: S.IntSet
    , whites :: S.IntSet
    , king :: Int
    }
  deriving (Show)


