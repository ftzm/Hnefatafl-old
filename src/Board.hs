{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances  #-}

module Board
  (Piece
    (White,Black,King,Empty,Corner)
  ,Direction
    (North,East,South,West)
  ,Coord
  ,Square
  ,Board
  ,getSquare
  ,getPiece
  ,blacks
  ,whites
  ,king
  ,xyToInt
  ,startBoard
  ,toEdge
  ,fromEdge
  ,go
  ,throne
  ,cornerCoords
  ,deletePieceBatch
  ,deletePiece
  ,putPiece
  )
where

import qualified Data.IntSet as S
import Data.List
import GHC.Generics
import Data.Aeson
import qualified Data.Map.Strict as M

data Piece = White | Black | King | Empty | Corner
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Direction = North | South | East | West
  deriving (Show, Eq)

type Coord = Int
type Square = (Coord,Piece)

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
  deriving (Show,Generic,ToJSON,FromJSON)

instance (ToJSON v) => ToJSON (M.Map Int v) where
    toJSON = toJSON . M.mapKeys show

instance (FromJSON v) => FromJSON (M.Map Int v) where
    parseJSON = fmap (M.mapKeys read) . parseJSON

startBoard :: Board
startBoard = Board {blacks=S.fromList blackStart
                   ,whites=S.fromList whiteStart
                   , king=throne
                   }

getPiece :: Board -> Int -> Piece
getPiece b i
  | S.member i $ blacks b = Black
  | S.member i $ whites b = White
  | i `elem` cornerCoords = Corner
  | i == king b = King
  | otherwise = Empty

putPiece :: Coord -> Piece -> Board -> Board
putPiece i Black b = b {blacks = S.insert i $ blacks b}
putPiece i White b = b {whites = S.insert i $ whites b}
putPiece i King b = b {king=i}

putPieceBatch :: Board -> [Square] ->  Board
putPieceBatch = foldl' (\b (x,y) -> putPiece x y b)

deletePiece :: Square -> Board -> Board
deletePiece (i,Black) b = b {blacks = S.delete i $ blacks b}
deletePiece (i,White) b = b {whites = S.delete i $ whites b}
deletePiece (_,King) b = b

deletePieceBatch :: [Square] -> Board -> Board
deletePieceBatch ss b = foldl' (flip deletePiece) b ss

getSquare :: Board -> Int -> (Int,Piece)
getSquare b i = (i,getPiece b i)

ifMaybe' :: a -> (a -> Bool) -> Maybe a
ifMaybe' x f | f x = Just x
             | otherwise = Nothing
go :: Board -> (Int,Piece) -> Direction -> Maybe (Int,Piece)
go b (i,_) d = getSquare b . a <$> ifMaybe' i t
  where (a,t) | North <- d = (,) (subtract 11) (>=11)
              | South <- d = (,) (+11)         (<=109)
              | East  <- d = (,) (+1)          ((/=10) . (`mod`11))
              | West  <- d = (,) (subtract 1)  ((/=0)  . (`mod`11))

toEdge :: (Int,Piece) -> Direction -> [Int]
toEdge (i,_) North = take (div i 11) $ tail $ iterate (subtract 11) i
toEdge (i,_) South = take (div (120-i) 11) $ tail $ iterate (+11) i
toEdge (i,_) East  = take (10 - mod i 11) $ tail $ iterate (+1) i
toEdge (i,_) West  = take (mod i 11) $ tail $ iterate (subtract 1) i

fromEdge :: Square -> Maybe Direction
fromEdge (x,_)
  | x < 11 = Just South
  | x > 109 = Just North
  | mod x 11 == 0 = Just East
  | mod x 11 == 10 = Just West
  | otherwise = Nothing
