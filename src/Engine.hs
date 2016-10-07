{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Engine
--  ( GameState(board
--             ,whiteTurn
--             ,whiteIsHuman
--             ,blackIsHuman
--             ,whiteLosses
--             ,blackLosses
--             ,lastMove
--             )
--  , Moves
--  , WinLose (Escape, KingCapture, NoMoves, NoPieces)
--  , PostTurn
--  , whiteCoords --iffy
--  , blackCoords --iffy
--  , movePiece --should be exported
--  , getSquare --should be exported
--  , getPiece --should be exported
--  , allMoves --should be exported
--  , sEq --should be exported
--  , startGame --should be exported
--  , runTurn --should be exported
--  --, intToCoord --for ai
--  --, coordToIntRaw --for ai
--  , whitePiece --for ai
--  , blackPiece --for ai
--  , startMoves
--  ) where
where

import Data.Aeson
import Data.Monoid
import Data.Aeson.TH
import GHC.Generics
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import qualified Data.IntSet as S
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import Board
import Moves

startGame :: GameState
startGame = GameState {board = startBoard
                      ,whiteIsHuman = True
                      ,blackIsHuman = True
                      , whiteTurn = True
                      ,lastMove = ((5,Black),(5,Black))
                      ,whiteLosses = 0
                      ,blackLosses = 0
                      ,whiteMoves = startMovesWhite
                      ,blackMoves = startMovesBlack
                      }

startMovesWhite :: Moves
startMovesWhite = allMovesSplit startGame

startMovesBlack :: Moves
startMovesBlack = allMovesSplit $ startGame {whiteTurn = False}

whitePiece :: Piece -> Bool
whitePiece White = True
whitePiece King = True
whitePiece _ = False

blackPiece :: Piece -> Bool
blackPiece Black = True
blackPiece _ = False

foes :: Square -> Square -> Bool
foes (_,p1) (c2,p2)
  | c2 `elem` cornerCoords = True
  | whitePiece p1 = blackPiece p2
  | blackPiece p1 = whitePiece p2
  | p1 == Black && c2 == throne = True
  | whitePiece p1 && (c2,p2) == (throne,Empty) = True
  | otherwise    = False

mobileFoes :: Square -> Square -> Bool
mobileFoes (_,p1) (_,p2)
  | whitePiece p1 = blackPiece p2
  | blackPiece p1 = whitePiece p2
  | otherwise    = False

friends :: Square -> Square -> Bool
friends (_,a) (_,b) | whitePiece a = whitePiece b
                    | blackPiece a = blackPiece b
                    | otherwise    = False

sEq :: Square -> Square -> Bool
sEq x y = snd x == snd y

opp :: Direction -> Direction
opp North = South
opp South = North
opp East  = West
opp West  = East

dirs :: [Direction]
dirs = [North, East, South, West]

perp :: Direction -> [Direction]
perp d | d == North || d == South = [East,West]
       | d == East  || d == West = [North,South]


ifMaybe' :: a -> (a -> Bool) -> Maybe a
ifMaybe' x f | f x = Just x
             | otherwise = Nothing

------------------------------------------------------------
--- Dealing with Moves
------------------------------------------------------------


------------------------------------------------------------
------------------------------------------------------------

------------------------------------------------------------
--- Captures
------------------------------------------------------------

ifMaybe :: a -> Bool -> Maybe a
ifMaybe x True = Just x
ifMaybe _ False = Nothing

--given the board, square which may be taken and the direction to get there
takePawn :: Board -> Direction -> Square -> Maybe [Square]
takePawn _ _ (_,King) = Nothing
takePawn b d s = ifMaybe [s] =<< (foes s <$> go b s d)

around :: Board -> Square -> [(Square, Direction)]
around b s = mapMaybe (\x -> (,x) <$> go b s x) dirs

takeKing :: Board -> Square -> Maybe [Square]
takeKing b s
  | length (filter (foes s . fst) $ around b s) == 4 = Just [s]
  | otherwise = Nothing

takeWhileIncl :: (a -> Bool) -> [a] -> [a]
takeWhileIncl _ [] = []
takeWhileIncl p (x:xs)
  | p x = x : takeWhileIncl p xs
  | otherwise = [x]

maybeInit :: [a] -> Maybe [a]
maybeInit [] = Nothing
maybeInit xs = Just $ init xs

gatherCaps :: Board -> Square -> Direction -> Maybe [Square]
gatherCaps b s d
  | null squares = Nothing
  | length squares < 2 = Nothing
  | foes s $ last squares = maybeInit squares
  | otherwise = Nothing
    where squares = takeWhileIncl (sEq s) $ map (getSquare b) $ toEdge s d

shieldWall :: Board -> Square -> Maybe [Square]
shieldWall _ (_,King) = Nothing
shieldWall b s = liftM2 (>>=) row surrounded =<< fromEdge s
    where
      row d = ((s:) . concat) <$> mapM (gatherCaps b s) (perp d)
      surrounded d = mapM (\x -> ifMaybe x (foes x (fromJust $ go b x d)))

captures :: Board -> Square -> Direction -> Maybe [Square]
captures b s d = takePawn b d s <|> shieldWall b s <|> takeKing b s
