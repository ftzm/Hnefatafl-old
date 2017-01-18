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
  ,xyToInt
  ,intToXY
  ,startBoard
  ,toEdge
  ,fromEdge
  ,go
  ,throne
  ,cornerCoords
  ,deletePieceBatch
  ,deletePiece
  ,putPiece
  ,whiteStart
  ,blackStart
  )
where

import Data.List
import Data.Int
import GHC.Generics
import Data.Maybe
import qualified Data.Vector.Unboxed as V

data Piece = White | Black | King | Empty | Corner
  deriving (Show, Eq, Generic)

data Direction = North | South | East | West
  deriving (Show, Eq)

type Coord = (Int, Int)
type Square = (Coord,Piece)

boardSize :: Int
boardSize = 11

xyToInt :: Coord -> Int
xyToInt (x,y) = boardSize * y + x

intToXY :: Int -> Coord
intToXY x = (\(y,x) -> (x,y)) $ divMod x 11

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

--data Board' = Board'
--    { blacks :: S.IntSet
--    , whites :: S.IntSet
--    , king :: Coord
--    }
--  deriving (Show)

type Board = V.Vector Int8

--startBoard :: Board
--startBoard = Board {blacks=S.fromList $ map xyToInt blackStart
--                   ,whites=S.fromList $ map xyToInt whiteStart
--                   , king=throne
--                   }

emptyBoard :: Board
emptyBoard = V.fromList $ replicate 121 0

startBoard :: Board
startBoard = foldl' putPieceBatch emptyBoard [bs, ws, cs, k]
  where
    bs = zip blackStart (repeat Black)
    ws = zip whiteStart (repeat White)
    cs = zip cornerCoords (repeat Corner)
    k = [(throne,King)]

--getPiece :: Board -> Coord -> Piece
--getPiece b i
--  | S.member (xyToInt i) $ blacks b = Black
--  | S.member (xyToInt i) $ whites b = White
--  | i `elem` cornerCoords = Corner
--  | i == king b = King
--  | otherwise = Empty

int8ToPiece :: Int8 -> Piece
int8ToPiece i
  | i == 0 = Empty
  | i == 1 = Black
  | i == 2 = White
  | i == 3 = Corner
  | i == 4 = King

pieceToInt8 :: Piece -> Int8
pieceToInt8 p
  | p == Empty  = 0
  | p == Black  = 1
  | p == White  = 2
  | p == Corner = 3
  | p == King   = 4

getPiece :: Board -> Coord -> Piece
getPiece b c = int8ToPiece $ b V.! xyToInt c

--putPiece :: Coord -> Piece -> Board -> Board
--putPiece i Black b = b {blacks = S.insert (xyToInt i) $ blacks b}
--putPiece i White b = b {whites = S.insert (xyToInt i) $ whites b}
--putPiece i King b = b {king=i}

putPiece :: Coord -> Piece -> Board -> Board
putPiece c p b = putPieceBatch b [(c,p)]

--putPieceBatch :: Board -> [Square] ->  Board
--putPieceBatch = foldl' (\b (x,y) -> putPiece x y b)

putPieceBatch :: Board -> [Square] ->  Board
putPieceBatch b ss = b V.// (map convert ss)
  where
    convert (c, p) = (xyToInt c, pieceToInt8 p)

--deletePiece :: Square -> Board -> Board
--deletePiece (i,Black) b = b {blacks = S.delete (xyToInt i) $ blacks b}
--deletePiece (i,White) b = b {whites = S.delete (xyToInt i) $ whites b}
--deletePiece (_,King) b = b

deletePiece :: Square -> Board -> Board
deletePiece (c,_) b = putPiece c Empty b

--deletePieceBatch :: [Square] -> Board -> Board
--deletePieceBatch ss b = foldl' (flip deletePiece) b ss

deletePieceBatch :: [Square] -> Board -> Board
deletePieceBatch ss b = putPieceBatch b $ zip (map fst ss) (repeat Empty)

getSquare :: Board -> Coord -> (Coord,Piece)
getSquare b i = (i,getPiece b i)

ifMaybe' :: a -> (a -> Bool) -> Maybe a
ifMaybe' x f | f x = Just x
             | otherwise = Nothing

maybeCoord :: Direction -> Coord -> Maybe Coord
maybeCoord d (x,y)
  | North <- d = (x,) . subtract 1 <$> ifMaybe' y (0<)
  | South <- d = (x,) . (+1)       <$> ifMaybe' y (10>)
  | East  <- d = (,y) . (+1)       <$> ifMaybe' x (10>)
  | West  <- d = (,y) . subtract 1 <$> ifMaybe' x (0<)

go :: Board -> Square -> Direction -> Maybe Square
go b (c,_) d = getSquare b <$> maybeCoord d c

toEdge :: Square -> Direction -> [Coord]
toEdge (c,_) d = map fromJust $ takeWhile isJust $ iterate (maybeCoord d =<<) $ maybeCoord d c

fromEdge :: Square -> Maybe Direction
fromEdge ((x,y),_)
  | x == 0  = Just East
  | x == 10 = Just West
  | y == 0  = Just South
  | y == 10 = Just North
  | otherwise = Nothing
