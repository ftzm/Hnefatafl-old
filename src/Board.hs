{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveGeneric  #-}

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
  ,maybeCoord
  ,whitePiece
  ,blackPiece
  ,opp
  )

where

import Data.List
import Data.Int
import GHC.Generics
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving

-------------------------------------------------------------------------------
-- Direction

data Direction = North | South | East | West
  deriving (Show, Eq, Ord)

opp :: Direction -> Direction
opp North = South
opp South = North
opp East  = West
opp West  = East

-------------------------------------------------------------------------------
-- Piece

data Piece = White | Black | King | Empty | Corner
  deriving (Show, Eq, Generic)

-- Use some th magic so make the Piece adt unboxed for Vector
derivingUnbox "Piece"
    [t| Piece -> Int8 |]
    [| pieceToInt8 |]
    [| int8ToPiece |]

int8ToPiece :: Int8 -> Piece
int8ToPiece i
  | i == 0 = Empty
  | i == 1 = Black
  | i == 2 = White
  | i == 3 = Corner
  | i == 4 = King
int8ToPiece _ = Empty

pieceToInt8 :: Piece -> Int8
pieceToInt8 p
  | p == Empty  = 0
  | p == Black  = 1
  | p == White  = 2
  | p == Corner = 3
  | p == King   = 4
pieceToInt8 _ = 0

-------------------------------------------------------------------------------

type Coord = (Int, Int)
type Square = (Coord,Piece)
type Board = V.Vector Piece

-------------------------------------------------------------------------------
-- Important Coords

boardSize :: Int
boardSize = 11

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

-------------------------------------------------------------------------------

xyToInt :: Coord -> Int
xyToInt (x,y) = boardSize * y + x

intToXY :: Int -> Coord
intToXY i = (\(y,x) -> (x,y)) $ divMod i 11

emptyBoard :: Board
emptyBoard = V.fromList $ replicate 121 Empty

startBoard :: Board
startBoard = foldl' putPieceBatch emptyBoard [bs, ws, cs, k]
  where
    bs = zip blackStart (repeat Black)
    ws = zip whiteStart (repeat White)
    cs = zip cornerCoords (repeat Corner)
    k = [(throne,King)]

getPiece :: Board -> Coord -> Piece
getPiece b c = b V.! xyToInt c

putPiece :: Coord -> Piece -> Board -> Board
putPiece c p b = putPieceBatch b [(c,p)]

putPieceBatch :: Board -> [Square] -> Board
putPieceBatch b ss = b V.// map convert ss
  where
    convert (c, p) = (xyToInt c, p)

deletePiece :: Square -> Board -> Board
deletePiece (c,_) = putPiece c Empty

deletePieceBatch :: [Square] -> Board -> Board
deletePieceBatch ss b = putPieceBatch b $ zip (map fst ss) (repeat Empty)

getSquare :: Board -> Coord -> (Coord,Piece)
getSquare b i = (i,getPiece b i)

ifMaybe :: a -> (a -> Bool) -> Maybe a
ifMaybe x f | f x = Just x
            | otherwise = Nothing

maybeCoord :: Direction -> Coord -> Maybe Coord
maybeCoord d (x,y)
  | North <- d = (x,) . subtract 1 <$> ifMaybe y (0<)
  | South <- d = (x,) . (+1)       <$> ifMaybe y (10>)
  | East  <- d = (,y) . (+1)       <$> ifMaybe x (10>)
  | West  <- d = (,y) . subtract 1 <$> ifMaybe x (0<)

go :: Board -> Square -> Direction -> Maybe Square
go b (c,_) d = getSquare b <$> maybeCoord d c

toEdge :: Square -> Direction -> [Coord]
toEdge (c,_) d = map fromJust $ takeWhile isJust
               $ iterate (maybeCoord d =<<) $ maybeCoord d c

toEdge' :: Square -> Direction -> [Coord]
toEdge' ((x,y),_) d
  | North <- d = reverse $ init [(x,y') | y' <- [0..y]]
  | East <- d = tail [(x',y) | x' <- [x..10]]
  | South <- d = tail [(x,y') | y' <- [y..10]]
  | West <- d = reverse $ init [(x',y) | x' <- [0..x]]

fromEdge :: Square -> Maybe Direction
fromEdge ((x,y),_)
  | x == 0  = Just East
  | x == 10 = Just West
  | y == 0  = Just South
  | y == 10 = Just North
  | otherwise = Nothing

whitePiece :: Piece -> Bool
whitePiece White = True
whitePiece King = True
whitePiece _ = False

blackPiece :: Piece -> Bool
blackPiece Black = True
blackPiece _ = False
