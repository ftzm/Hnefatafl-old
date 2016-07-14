-----------todo
--accept options at startup regarding who is ai and who not.

--separate files into different modules

--convert ratio to score when printing board
--consider passing sqaures all the way through to movePiece instead of making it look them up again
--loss if can't make a move
--consider

--better appearance of board
-- +-----+
-- | /`\ |
-- | \_/ |
-- +-----+
--       A   B   C   D   E   F   G   H   I   J   K
--     +---+---+---+---+---+---+---+---+---+---+---+
--  1  |   |   |   | X | X | X | X | X |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
--  2  |   |   |   | O |   | X |   |   |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
--  3  |   |   |   |   |   |   |   |   |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
--  4  |   |   |   |   |   |   |   |   |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
--  5  |   |   |   |   |   |OOO|   |   |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
--  6  |   |   |   |   |   |   |   |   |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
--  7  |   |   |   |   |   |   |   |   |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
--  8  |   |   |   |   |   |   |   |   |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
--  9  |   |   |   |   |   |   |   |   |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
-- 10  |   |   |   |   |   | X |   |   |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+
-- 11  |   |   |   | X | X | X | X | X |   |   |   |
--     +---+---+---+---+---+---+---+---+---+---+---+

--pause before ai move

-- mechanism for randomly choosing an ai strategy
-- pass in difficulty to ai

--implement loss when black has no more pieces

--find better way to handle if there is no board or ratio chance on piece move than
--resetting the same board and ratio on gamestate

{-# LANGUAGE TupleSections #-}

module Hnefatafl
    ( runGameLoop
    ) where

import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Vector as V
import System.IO
import System.Console.ANSI (clearScreen)
import Control.Applicative
import Control.Exception --for withEcho
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Arrow

data Piece = White | Black | King | Empty | Corner
  deriving (Show, Eq)
data Direction = North | South | East | West
  deriving (Show, Eq)
type Coord = (Int,Int)
type Square = (Coord,Piece)
type Board = IM.IntMap Piece

data GameState = GameState
    { board :: Board
    , ratio :: Int
    , whiteIsHuman :: Bool
    , blackIsHuman :: Bool
    , frontTurn :: Bool
    , continue :: Bool
    }
  deriving (Show)

startGame :: GameState
startGame = GameState startBoard 0 True False True True

whitePiece :: Piece -> Bool
whitePiece White = True
whitePiece King = True
whitePiece _ = False

blackPiece :: Piece -> Bool
blackPiece Black = True
blackPiece _ = False

foes :: Square -> Square -> Bool
foes _         ((0,0)  ,_)      = True --Corner
foes _         ((0,10) ,_)      = True --Corner
foes _         ((10,0) ,_)      = True --Corner
foes _         ((10,10),_)      = True --Corner
foes (_,Black) ((5,5)  ,_)      = True --Throne
foes (_,White) ((5,5),Empty)    = True --Throne
foes (_,King)  ((5,5),Empty)    = True --Throne

foes (_,a) (_,b) | whitePiece a = blackPiece b
                 | blackPiece a = whitePiece b
                 | otherwise    = False

friends :: Square -> Square -> Bool
friends (_,a) (_,b) | whitePiece a = whitePiece b
                    | blackPiece a = blackPiece b
                    | otherwise    = False

opp :: Direction -> Direction
opp North = South
opp South = North
opp East  = West
opp West  = East

dirs :: [Direction]
dirs = [North, South, East, West]

perp :: Direction -> [Direction]
perp d | d == North || d == South = [East,West]
       | d == East  || d == West = [North,South]

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

coordToIntRaw :: Coord -> Int
coordToIntRaw (x,y) = boardSize * y + x

coordToInt :: Coord -> Maybe Int
coordToInt c@(x,y)
  | x < 0 || x > 10 || y < 0 || y > 10 = Nothing
  | otherwise = Just $ coordToIntRaw c

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

intToCoord :: Int -> Coord
intToCoord x = swap $ divMod x boardSize

startBoard :: Board
startBoard = IM.fromList (whites ++ blacks ++ corners ++ king )
  where
    whites = zip (map coordToIntRaw whiteStart) (repeat White)
    blacks = zip (map coordToIntRaw blackStart) (repeat Black)
    corners = zip (map coordToIntRaw cornerCoords) (repeat Corner)
    king = [(coordToIntRaw kingStart,King)]

boardToString :: Board -> String
boardToString m = map (symbol . (`getPiece` m) . coordToIntRaw)
              [(x,y) | y <- [0..10], x <- [0..10]]
  where
    symbol Black  = 'X'
    symbol White  = '0'
    symbol King   = '1'
    symbol Empty  = '_'
    symbol Corner = ' '

boardStringKeys :: [Coord] -> String -> String
boardStringKeys ks b = V.toList $ V.fromList b V.// zip (map coordToIntRaw ks) ['a'..'z']

alphaToIndex :: Char -> [Coord] -> Maybe Int
alphaToIndex c xs = elemIndex c $ zipWith const ['a'..'z'] xs

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

printBoardString :: String -> IO ()
printBoardString = putStrLn . (++"\n") . intercalate "\n"
                 . splitEvery 22 . intersperse ' '

getPiece :: Int -> Board -> Piece
getPiece = IM.findWithDefault Empty

putPiece :: Coord -> Piece -> Board -> Board
putPiece c = IM.insert (coordToIntRaw c)

getSquare :: Coord -> Board -> Maybe Square
getSquare c m = (c,) . (`getPiece` m) <$> coordToInt c

go :: Board -> Direction -> Square -> Maybe Square
go b d ((x,y),_) = getSquare (c d) b
  where c North = (x,y-1)
        c South = (x,y+1)
        c East  = (x+1,y)
        c West  = (x-1,y)

go' b s d = go b d s

toEdge :: Board -> Square -> Direction -> [Square]
toEdge b s d = tail . catMaybes . takeWhile (/=Nothing)
            $ iterate (go b d =<<) (return s)

pieceMoves :: Board -> Square -> [(Coord, Coord)]
pieceMoves b s@(c,p) = concatMap (map ((c,) . fst) . takeWhile (eligible . snd) . toEdge b s) dirs
  where eligible x | p /= King = x == Empty
                   | p == King = x == Empty || x == Corner

--given the board, square which may be taken and the direction to get there
takePawn :: Board -> Direction -> Square -> Maybe [Square]
takePawn b d (_,King) = Nothing
takePawn b d s = (foes s <$> go b d s)
               >>= \x -> if x then Just [s] else Nothing

--given the board, square which may be taken and the direction to get there
takeKing :: Board -> Square -> Maybe [Square]
takeKing b s
  | length ( filter (foes s) $ mapMaybe (\x -> go b x s) dirs) == 4 = Just [s]
  | otherwise = Nothing

--shieldWall :: Board -> Direction -> Square -> Maybe [Square]

captures :: Board -> Direction -> Square -> Maybe [Square]
captures b d s = takePawn b d s <|> takeKing b s

--coordinate pawn takes, shieldwalls, and king takes
takePieces :: Square -> Board -> Int -> Maybe (Board,Int)
takePieces s b r
  | null taken = Nothing
  | otherwise = Just (delete taken,ratio)
  where
    ratio
      | King `elem` map snd taken = -100
      | otherwise = if whitePiece $ snd $ head taken
                      then r - length taken
                      else r + length taken
    delete = foldl' (\b (k,_) -> IM.insert (coordToIntRaw k) Empty b ) b
    taken = concat $ mapMaybe (uncurry (captures b)) adjFoes
    adjFoes = filter (foes s . snd) $ mapMaybe (\x -> (x,) <$> go b x s) dirs

moveEffect :: Square -> Board -> Int -> (Board,Int)
moveEffect s@(c,p) b r
  | p == King && c `elem` cornerCoords = (b,100)
  | otherwise = fromMaybe (b,r) $ takePieces s b r

movePiece :: GameState -> (Coord,Coord) -> GameState
movePiece g (c1,c2) = g {board = newB, ratio = newR, frontTurn = not ft}
  where
    b = board g
    r = ratio g
    ft = frontTurn g
    (newB, newR) = moveEffect (c2,v1) (putPiece c2 v1 $ putPiece c1 v2 b) r
    (_,v1) = fromJust $ getSquare c1 b
    (_,v2) = fromJust $ getSquare c2 b

allMoves :: GameState -> [GameState]
allMoves g = map (movePiece g) $ concatMap (pieceMoves (board g) . first intToCoord) squares
  where
    squares
      | frontTurn g = IM.assocs $ IM.filter whitePiece $ board g
      | otherwise = IM.assocs $ IM.filter blackPiece $ board g
    toGameState (b,r) = g {board = b, ratio = r}

bestMove :: Int -> GameState -> GameState
bestMove i g
  | i == 0 = best (comparing ratio) $ allMoves g
  | otherwise = best (comparing (ratio . bestMove (i-1) . bestMove 0)) $ allMoves g
--  | otherwise = bestMove 0 g
  where
    best = if frontTurn g then maximumBy else minimumBy

selectByKey :: [Coord] -> Board -> MaybeT IO Coord
selectByKey xs b = do
  lift clearScreen
  lift $ printBoardString $ boardStringKeys xs $ boardToString b
  char <- lift $ withoutBuffering $ withoutEcho getChar
  index <- MaybeT $ return $ alphaToIndex char xs
  return (xs !! index)

selectMove :: [Coord] -> Board -> IO (Coord,Coord)
selectMove xs b = do
  piece' <- runMaybeT $ selectByKey xs b
  case piece' of
    Nothing -> putStrLn "Invalid choice" >> selectMove xs b
    Just piece -> do
      let square = fromJust $ getSquare piece b
      move' <- runMaybeT $ selectByKey (map snd $ pieceMoves b square) b
      case move' of
        Nothing -> selectMove xs b
        Just move -> return (piece,move)

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
  printBoardString $ boardToString b
  _ <- withoutBuffering $ withoutEcho getChar
  return ()

whiteCoords :: Board -> [Coord]
whiteCoords = map intToCoord . IM.keys . IM.filter whitePiece

blackCoords :: Board -> [Coord]
blackCoords = map intToCoord . IM.keys . IM.filter blackPiece

gameLoop :: IO GameState -> IO GameState
gameLoop g' = do
  g <- g'
  let b = board g
  let ft = frontTurn g
  let human = if ft then whiteIsHuman g else blackIsHuman g
  newGameState <- if human
     then do
        displayboard b
        let coords = if ft then whiteCoords b else blackCoords b
        move <- selectMove coords b
        let newGameState' = movePiece g move
        return newGameState'
     else do
        let ng = bestMove 1 g
        return ng

  if (ratio newGameState == -100) || (ratio newGameState == 100)
     then putStrLn "Victory" >> return newGameState
     else gameLoop $ return newGameState

runGameLoop :: IO GameState
runGameLoop = gameLoop $ return startGame
--main = gameLoop $ return startGame
--main = displayboard $ board $ bestMove 2 startGame -- for testing
