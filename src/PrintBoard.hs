module PrintBoard
  ( selectByKey
  , displayboard
  , pauseUntilKeypress
  , withoutBuffering
  ) where

import Board
import qualified Data.IntSet as S

import System.IO
import System.Console.ANSI (clearScreen)
import Control.Exception --for withEcho
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.List
import qualified Data.Vector as V
--import qualified Data.IntMap.Strict as IM

piecesToString :: Board -> String
piecesToString m = map (symbol . getPiece m) $ map intToXY [0..120]
  where
    symbol Black  = 'X'
    symbol White  = '0'
    symbol King   = '1'
    symbol Empty  = ' '
    symbol Corner = '+'

boardStringKeys :: [Coord] -> String -> String
boardStringKeys ks b = V.toList $ V.fromList b V.// zip (map xyToInt ks) ['a'..'z']

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

colorizeBoard :: [String] -> [String]
colorizeBoard = map (\x -> "\x1b[38;5;10m" ++ x ++ "\x1b[0m")

colorizePieces :: [String] -> [String]
colorizePieces = map rightColor
  where rightColor s
          | s == "X" = "\x1b[34m" ++ s ++ "\x1b[0m"
          | s == "0" = "\x1b[33m" ++ s ++ "\x1b[0m"
          | s == "1" = "\x1b[31m0\x1b[0m"
          | s == "+" = "\x1b[38;5;10m" ++ s ++ "\x1b[0m"
          | otherwise = "\x1b[35m" ++ s ++ "\x1b[0m"

drawBoard :: String -> IO ()
drawBoard s = putStrLn $ concat $ weave boardParts $ colorizePieces $ map (:[]) s
  where numberParts = map betweenPieceRows numbers
        boardParts = colorizeBoard $ topSection ++ concat numberParts ++ [" |\n" ++ horiLine]

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

pauseUntilKeypress :: IO ()
--pauseUntilKeypress = withoutBuffering $ withoutEcho getChar >> return ()
pauseUntilKeypress = void $ withoutBuffering $ withoutEcho getChar

displayboard :: Board -> IO ()
displayboard b = clearScreen >> drawBoard ( piecesToString b)

selectByKey :: [Coord] -> Board -> MaybeT IO Coord
selectByKey xs b = do
  lift clearScreen
  lift $ drawBoard $ boardStringKeys xs $ piecesToString b
  char <- lift $ withoutBuffering $ withoutEcho getChar
  index <- MaybeT $ return $ alphaToIndex char xs
  return (xs !! index)
