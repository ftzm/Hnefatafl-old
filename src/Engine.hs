{-# LANGUAGE TupleSections #-}
module Engine
  ( GameState(board,frontTurn,ratio,whiteIsHuman,blackIsHuman) --should be exported
  , whiteCoords --iffy
  , blackCoords --iffy
  , movePiece --should be exported
  , getSquare --should be exported
  , pieceMoves --should be exported
  , allMoves --should be exported
  , allMoves' --should be exported
  , startGame --should be exported
  , intToCoord --for ai
  , whitePiece --for ai
  , blackPiece --for ai
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import Control.Arrow

import BoardData

data GameState = GameState
    { board :: Board
    , ratio :: Int
    , whiteIsHuman :: Bool
    , blackIsHuman :: Bool
    , frontTurn :: Bool
--    , continue :: Bool
    }
  deriving (Show)

startGame :: GameState
startGame = GameState startBoard 0 True True True --True

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

--friends :: Square -> Square -> Bool
--friends (_,a) (_,b) | whitePiece a = whitePiece b
--                    | blackPiece a = blackPiece b
--                    | otherwise    = False

sEq :: Square -> Square -> Bool
sEq x y = snd x == snd y

--opp :: Direction -> Direction
--opp North = South
--opp South = North
--opp East  = West
--opp West  = East

dirs :: [Direction]
dirs = [North, South, East, West]

perp :: Direction -> [Direction]
perp d | d == North || d == South = [East,West]
       | d == East  || d == West = [North,South]

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

getPiece :: Int -> Board -> Piece
getPiece = IM.findWithDefault Empty

putPiece :: Coord -> Piece -> Board -> Board
putPiece c = IM.insert (coordToIntRaw c)

putPieceBatch :: Board -> [Square] ->  Board
putPieceBatch = foldl' (\b (x,y) -> putPiece x y b)

getSquare :: Coord -> Board -> Maybe Square
getSquare c m = (c,) . (`getPiece` m) <$> coordToInt c

go :: Board -> Direction -> Square -> Maybe Square
go b d ((x,y),_) = getSquare (c d) b
  where c North = (x,y-1)
        c South = (x,y+1)
        c East  = (x+1,y)
        c West  = (x-1,y)

toEdge :: Board -> Square -> Direction -> [Square]
toEdge b s d = tail . catMaybes . takeWhile (/=Nothing)
            $ iterate (go b d =<<) (Just s)

pieceMoves :: Board -> Square -> [(Coord, Coord)]
pieceMoves b s@(c,p) = concatMap (map ((c,) . fst) . takeWhile (eligible . snd) . toEdge b s) dirs
  where eligible x | p /= King = x == Empty
                   | p == King = x == Empty || x == Corner

pieceMoves' :: Board -> Square -> [Coord]
pieceMoves' b s@(_,p) = concatMap (map fst . takeWhile (eligible . snd) . toEdge b s) dirs
  where eligible x | p /= King = x == Empty
                   | p == King = x == Empty || x == Corner

allMoves :: GameState -> [GameState]
allMoves g = map (movePiece g) $ concatMap (pieceMoves (board g) . first intToCoord) squares
  where
    squares
      | frontTurn g = IM.assocs $ IM.filter whitePiece $ board g
      | otherwise = IM.assocs $ IM.filter blackPiece $ board g

allMoves' :: GameState -> M.Map Coord [Coord]
allMoves' g = foldl' buildMap M.empty squares
  where
    buildMap acc s@(x,_)
      | null $ pieceMoves (board g) s = acc
      | otherwise = M.insert x (pieceMoves' (board g) s) acc
    pType = if frontTurn g then whitePiece else blackPiece
    squares = map (first intToCoord) $ IM.assocs $ IM.filter pType $ board g

ifMaybe :: a -> Bool -> Maybe a
ifMaybe x True = Just x
ifMaybe _ False = Nothing

--given the board, square which may be taken and the direction to get there
takePawn :: Board -> Direction -> Square -> Maybe [Square]
takePawn _ _ (_,King) = Nothing
takePawn b d s = ifMaybe [s] =<< (foes s <$> go b d s)

around :: Board -> Square -> [(Square, Direction)]
around b s = mapMaybe (\x -> (,x) <$> go b x s) dirs

takeKing :: Board -> Square -> Maybe [Square]
takeKing b s
  | length (filter (foes s . fst) $ around b s) == 4 = Just [s]
  | otherwise = Nothing

-- |if square is at the edge of the board then the direction inward
-- as Just Direction, otherwise Nothing
fromEdge :: Square -> Maybe Direction
fromEdge ((0,_),_) = Just East
fromEdge ((10,_),_) = Just West
fromEdge ((_,0),_) = Just South
fromEdge ((_,10),_) = Just North
fromEdge _ = Nothing

takeWhileIncl :: (a -> Bool) -> [a] -> [a]
takeWhileIncl _ [] = []
takeWhileIncl p (x:xs)
  | p x = x : takeWhileIncl p xs
  | otherwise = [x]

maybeInit :: [a] -> Maybe [a]
maybeInit [] = Nothing
maybeInit xs = Just $ init xs

--bookendedRow :: Board -> Square -> Direction -> Maybe [Square]
--bookendedRow =

gatherCaps :: Board -> Square -> Direction -> Maybe [Square]
gatherCaps b s d
  | null squares = Nothing
  | foes s $ last squares = maybeInit squares
  | otherwise = Nothing
    where squares = takeWhileIncl (sEq s) $ toEdge b s d

shieldWall :: Board -> Square -> Maybe [Square]
shieldWall _ (_,King) = Nothing
shieldWall b s = liftM2 (>>=) row surrounded =<< fromEdge s
    where
      row d = ((s:) . concat) <$> mapM (gatherCaps b s) (perp d)
      surrounded d = mapM (\x -> ifMaybe x (foes x (fromJust $ go b d x)))

captures :: Board -> Square -> Direction -> Maybe [Square]
captures b s d = takePawn b d s <|> shieldWall b s <|> takeKing b s

--coordinate pawn takes, shieldwalls, and king takes
--TODO make not horrendous
takePieces :: Square -> Board -> Int -> Maybe (Board,Int)
takePieces s b r
  | null taken = Nothing
  | otherwise = Just (deletePieces taken,newRatio)
  where
    newRatio
      | King `elem` map snd taken = -100
      | otherwise = if whitePiece $ snd $ head taken
                      then r - length taken
                      else r + length taken
    deletePieces = putPieceBatch b . map ((,Empty) . fst)
    taken = concat $ mapMaybe (uncurry (captures b)) adjFoes
    adjFoes = filter (liftM2 (&&) (foes s) ((/=Corner) . snd) . fst) $ around b s

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

whiteCoords :: Board -> [Coord]
whiteCoords = map intToCoord . IM.keys . IM.filter whitePiece

blackCoords :: Board -> [Coord]
blackCoords = map intToCoord . IM.keys . IM.filter blackPiece

