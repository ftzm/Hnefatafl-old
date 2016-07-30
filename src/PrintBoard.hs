module PrintBoard
  ( selectByKey
  , displayboard
  ) where

import BoardData

import System.IO
import System.Console.ANSI (clearScreen)
import Control.Exception --for withEcho
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.List
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM

getPiece :: Int -> Board -> Piece
getPiece = IM.findWithDefault Empty

coordToIntRaw :: Coord -> Int
coordToIntRaw (x,y) = boardSize * y + x

piecesToString :: Board -> String
piecesToString m = map (symbol . (`getPiece` m) . coordToIntRaw)
              [(x,y) | y <- [0..10], x <- [0..10]]
  where
    symbol Black  = 'X'
    symbol White  = '0'
    symbol King   = '1'
    symbol Empty  = ' '
    symbol Corner = '+'

boardStringKeys :: [Coord] -> String -> String
boardStringKeys ks b = V.toList $ V.fromList b V.// zip (map coordToIntRaw ks) ['a'..'z']

alphaToIndex :: Char -> [Coord] -> Maybe Int
alphaToIndex c xs = elemIndex c $ zipWith const ['a'..'z'] xs

--splitEvery :: Int -> [a] -> [[a]]
--splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

duplicate :: Int -> String -> String
duplicate = (>>) . enumFromTo 1

letters :: [String]
letters = map (:[]) ['A'..'K']

numbers :: [String]
numbers = map ((' ':) . show) ([2..9] :: [Int]) ++ ["10","11"]

--couch :: [String] -> String -> String
--couch (x:xs) y = y ++ x ++ couch xs y
--couch [] y = y

topSection :: [String]
topSection = (intercalate "   " ("   ":letters) ++ "\n" ++ horiLine ++"  1 | ")
           : replicate 10 " | "

horiLine :: String
horiLine = "    +" ++ duplicate 11 "---+" ++ "\n"

betweenPieceRows :: String -> [String]
betweenPieceRows x = (" |\n" ++ horiLine ++ " " ++ x ++ " | ")
                   : replicate 10 " | "

weave :: [a] -> [a] -> [a]
weave (x:xs) ys = x : weave ys xs
weave [] (y:_) = [y]
weave [] [] = []

drawBoard :: String -> IO ()
drawBoard s = putStrLn $ concat $ weave ( topSection ++ concat boardParts ++ [" |\n" ++ horiLine]) $ map (:[]) s
  where boardParts = map betweenPieceRows numbers

--don't show typing in terminal
withoutEcho :: IO a -> IO a
withoutEcho action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action

--don't wait for return
withoutBuffering :: IO a -> IO a
withoutBuffering action = do
  old <- hGetBuffering stdin
  bracket_ (hSetBuffering stdin NoBuffering) (hSetBuffering stdin old) action

displayboard :: Board -> IO ()
displayboard b = do
  clearScreen
  drawBoard $ piecesToString b
  _ <- withoutBuffering $ withoutEcho getChar
  return ()

selectByKey :: [Coord] -> Board -> MaybeT IO Coord
selectByKey xs b = do
  lift clearScreen
  lift $ drawBoard $ boardStringKeys xs $ piecesToString b
  char <- lift $ withoutBuffering $ withoutEcho getChar
  index <- MaybeT $ return $ alphaToIndex char xs
  return (xs !! index)
