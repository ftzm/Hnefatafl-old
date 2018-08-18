{-# LANGUAGE TupleSections #-}

module Capture
  (findCaptures'
  ,foes
  ,friends
  ,ifMaybe
  ,sEq
  ,perp
  ,whitePiece
  ,blackPiece
  ,takeWhileIncl
  )
where

import Data.Maybe
import Control.Monad
import Control.Applicative

import Board

perp :: Direction -> [Direction]
perp d | d == North || d == South = [East,West]
       | otherwise = [North,South]

sEq :: Square -> Square -> Bool
sEq x y = snd x == snd y

friends :: Square -> Square -> Bool
friends (_,a) (_,b) | whitePiece a = whitePiece b
                    | blackPiece a = blackPiece b
                    | otherwise    = False

foes :: Square -> Square -> Bool
foes (_,p1) (c2,p2)
  | c2 `elem` cornerCoords = True
  | whitePiece p1 = blackPiece p2
  | blackPiece p1 = whitePiece p2
  | p1 == Black && c2 == throne = True
  | whitePiece p1 && (c2,p2) == (throne,Empty) = True
  | otherwise    = False

ifMaybe :: a -> Bool -> Maybe a
ifMaybe x True = Just x
ifMaybe _ False = Nothing

--given the board, square which may be taken and the direction to get there
takePawn :: Board -> Direction -> Square -> Maybe [Square]
takePawn _ _ (_,King) = Nothing
takePawn b d s = ifMaybe [s] =<< (foes s <$> go b s d)

around :: Board -> Square -> [(Square, Direction)]
around b s = mapMaybe (\x -> (,x) <$> go b s x) [North,East,South,West]

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
      row d = (s:) . concat <$> mapM (gatherCaps b s) (perp d)
      surrounded d = mapM (\x -> ifMaybe x (foes x (fromJust $ go b x d)))

captures :: Board -> Square -> Direction -> Maybe [Square]
captures b s d = takePawn b d s <|> shieldWall b s <|> takeKing b s

findCaptures' :: Board -> Square -> [Square]
findCaptures' b s = concat $ mapMaybe (uncurry (captures b)) $ filter (liftM2 (&&) (foes s) ((/=Corner) . snd) . fst) $ around b s
