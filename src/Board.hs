{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TupleSections  #-}

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

type Coord = (Int, Int)
type Square = (Coord,Piece)

boardSize :: Int
boardSize = 11

xyToInt :: Coord -> Int
xyToInt (x,y) = boardSize * y + x

blackStart :: [Coord]
blackStart = [(3,0),(4,0),(5,0),(6,0),(7,0),(5,1),(0,3),(10,3),(0,4),(10,4),
              (0,5),(1,5),(9,5),(10,5),(0,6),(10,6),(0,7),(10,7),(5,9),(3,10),
              (4,10),(5,10),(6,10),(7,10)]

whiteStart :: [Coord]
whiteStart = [(5,3),(4,4),(5,4),(6,4),(3,5),(4,5),(6,5),(7,5),(4,6),(5,6),
              (6,6),(5,7)]

cornerCoords :: [Coord]
cornerCoords = [(0,0),(10,0),(0,10),(10,10)]

throne :: Coord
throne = (5,5)

data Board = Board
    { blacks :: S.IntSet
    , whites :: S.IntSet
    , king :: Coord
    }
  deriving (Show,Generic,ToJSON,FromJSON)

instance (ToJSON v) => ToJSON (M.Map Int v) where
    toJSON = toJSON . M.mapKeys show

instance (FromJSON v) => FromJSON (M.Map Int v) where
    parseJSON = fmap (M.mapKeys read) . parseJSON

startBoard :: Board
startBoard = Board {blacks=S.fromList $ map xyToInt blackStart
                   ,whites=S.fromList $ map xyToInt whiteStart
                   , king=throne
                   }

getPiece :: Board -> Coord -> Piece
getPiece b i
  | S.member (xyToInt i) $ blacks b = Black
  | S.member (xyToInt i) $ whites b = White
  | i `elem` cornerCoords = Corner
  | i == king b = King
  | otherwise = Empty

putPiece :: Coord -> Piece -> Board -> Board
putPiece i Black b = b {blacks = S.insert (xyToInt i) $ blacks b}
putPiece i White b = b {whites = S.insert (xyToInt i) $ whites b}
putPiece i King b = b {king=i}

putPieceBatch :: Board -> [Square] ->  Board
putPieceBatch = foldl' (\b (x,y) -> putPiece x y b)

deletePiece :: Square -> Board -> Board
deletePiece (i,Black) b = b {blacks = S.delete (xyToInt i) $ blacks b}
deletePiece (i,White) b = b {whites = S.delete (xyToInt i) $ whites b}
deletePiece (_,King) b = b

deletePieceBatch :: [Square] -> Board -> Board
deletePieceBatch ss b = foldl' (flip deletePiece) b ss

getSquare :: Board -> Coord -> (Coord,Piece)
getSquare b i = (i,getPiece b i)

ifMaybe' :: a -> (a -> Bool) -> Maybe a
ifMaybe' x f | f x = Just x
             | otherwise = Nothing

maybeCoord :: Coord -> Direction -> Maybe Coord
maybeCoord (x,y) d
  | North <- d = (x,) . subtract 1 <$> ifMaybe' x (0>)
  | South <- d = (x,) . (+1)       <$> ifMaybe' x (10<)
  | East  <- d = (,y) . subtract 1 <$> ifMaybe' y (0>)
  | West  <- d = (,y) . (+1)       <$> ifMaybe' y (10<)

go :: Board -> Square -> Direction -> Maybe Square
go b (c,_) d = getSquare b <$> maybeCoord c d

fromEdge :: Square -> Maybe Direction
fromEdge ((x,y),_)
  | x == 0  = Just East
  | x == 10 = Just West
  | y == 0  = Just South
  | y == 10 = Just North
  | otherwise = Nothing
