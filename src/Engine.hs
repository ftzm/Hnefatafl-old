{-# LANGUAGE TupleSections #-}
module Engine
  ( GameState(board,frontTurn,ratio,whiteIsHuman,blackIsHuman) --should be exported
  , whiteCoords --iffy
  , blackCoords --iffy
  , movePiece --should be exported
  , getSquare --should be exported
  , pieceMoves --should be exported
  , startGame --should be exported
  , intToCoord --for ai
  , whitePiece --for ai
  , blackPiece --for ai
  ) where

import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.List
import Control.Applicative

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

friends :: Square -> Square -> Bool
friends (_,a) (_,b) | whitePiece a = whitePiece b
                    | blackPiece a = blackPiece b
                    | otherwise    = False

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

getSquare :: Coord -> Board -> Maybe Square
getSquare c m = (c,) . (`getPiece` m) <$> coordToInt c

go :: Board -> Direction -> Square -> Maybe Square
go b d ((x,y),_) = getSquare (c d) b
  where c North = (x,y-1)
        c South = (x,y+1)
        c East  = (x+1,y)
        c West  = (x-1,y)

--go' b s d = go b d s

toEdge :: Board -> Square -> Direction -> [Square]
toEdge b s d = tail . catMaybes . takeWhile (/=Nothing)
            $ iterate (go b d =<<) (return s)

pieceMoves :: Board -> Square -> [(Coord, Coord)]
pieceMoves b s@(c,p) = concatMap (map ((c,) . fst) . takeWhile (eligible . snd) . toEdge b s) dirs
  where eligible x | p /= King = x == Empty
                   | p == King = x == Empty || x == Corner

ifMaybe :: a -> Bool -> Maybe a
ifMaybe x True = Just x
ifMaybe _ False = Nothing

--given the board, square which may be taken and the direction to get there
takePawn :: Board -> Direction -> Square -> Maybe [Square]
takePawn _ _ (_,King) = Nothing
--takePawn b d s = ifMaybe [s] =<< (foes s <$> go b d s)
takePawn b d s = ifMaybe [s] =<< (foes s <$> go b d s)

--given the board, square which may be taken and the direction to get there
takeKing :: Board -> Square -> Maybe [Square]
takeKing b s
  | length ( filter (foes s) $ mapMaybe (\x -> go b x s) dirs) == 4 = Just [s]
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

--TODO use safeinit or something
gatherCaps :: Board -> Square -> Direction -> Maybe [Square]
gatherCaps b s d
  | null squares = Nothing
  | foes s $ last squares = Just $ init squares
  | otherwise = Nothing
    where squares = takeWhileIncl (friends s) $ toEdge b s d

--TODO no king caveat
shieldWall :: Board -> Square -> Maybe [Square]
shieldWall b s
  | Just inward <- fromEdge s
  = mapM (\x -> ifMaybe x (foes x (fromJust $ go b inward x))) =<< row inward
  | otherwise = Nothing
    where
      row d = ((s:) . concat) <$> mapM (gatherCaps b s) (perp d)

captures :: Board -> Direction -> Square -> Maybe [Square]
captures b d s = takePawn b d s <|> shieldWall b s <|> takeKing b s

--coordinate pawn takes, shieldwalls, and king takes
--TODO corners arent foes
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
    deletePieces = foldl' (\b' (k,_) -> IM.insert (coordToIntRaw k) Empty b' ) b
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

whiteCoords :: Board -> [Coord]
whiteCoords = map intToCoord . IM.keys . IM.filter whitePiece

blackCoords :: Board -> [Coord]
blackCoords = map intToCoord . IM.keys . IM.filter blackPiece

