{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Engine
  ( GameState(board
             ,whiteTurn
             ,whiteIsHuman
             ,blackIsHuman
             ,whiteLosses
             ,blackLosses
             )
  , Moves
  , WinLose (Escape, KingCapture, NoMoves, NoPieces)
  , whiteCoords --iffy
  , blackCoords --iffy
  , movePiece --should be exported
  , getSquare --should be exported
  , getPiece --should be exported
  , allMoves --should be exported
  , sEq --should be exported
  , startGame --should be exported
  , runTurn --should be exported
  , intToCoord --for ai
  , coordToIntRaw --for ai
  , whitePiece --for ai
  , blackPiece --for ai
  , startMoves
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import Control.Arrow hiding (left)
import Control.Monad.State.Strict

import BoardData

data WinLose = KingCapture | Escape |  NoPieces | NoMoves
  deriving (Eq, Show)

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance (Monad m) => Applicative (EitherT e m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Functor (EitherT e m) where
  fmap = liftM

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance (Monad m) => Monad (EitherT e m) where
  return a = EitherT $ return $ Right a
  x >>= f = EitherT $ runEitherT x >>= either (return . Left) (runEitherT . f)

instance MonadState s m => MonadState s (EitherT e m) where
  get = lift get
  put = lift . put

class MonadEither e m where
  left :: e -> m a

instance MonadEither e (Either e) where
  left = Left

instance Monad m => MonadEither e (EitherT e m) where
  left = EitherT . return . left

newtype TurnT a = TurnT {runTurnT :: EitherT WinLose (State GameState) a}
  deriving (Applicative, Functor, Monad, MonadState GameState, MonadEither WinLose)

doTurnT :: TurnT a -> GameState -> (Either WinLose a, GameState)
doTurnT t = runState (runEitherT $ runTurnT t)

data GameState = GameState
    { board :: Board
    , whiteIsHuman :: Bool
    , blackIsHuman :: Bool
    , whiteTurn :: Bool
    , lastMove :: (Square,Square)
    , whiteLosses :: Int
    , blackLosses :: Int
    }
  deriving (Show)

toggleWhiteTurn :: GameState -> GameState
toggleWhiteTurn g = g {whiteTurn = not $ whiteTurn g}

writeBoard :: GameState -> Board -> GameState
writeBoard g b = g { board = b}

type Moves = M.Map Coord [Coord]

startGame :: GameState
startGame = GameState startBoard True True True (((5,0),Black),((0,5),Black)) 0 0 --True

startMoves :: Moves
startMoves = allMoves startGame

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

pieceMoves' :: Board -> Square -> [Coord]
pieceMoves' b s@(_,p) = concatMap (map fst . takeWhile (eligible . snd) . toEdge b s) dirs
  where eligible x | p /= King = x == Empty
                   | p == King = x == Empty || x == Corner

--allMoves :: GameState -> [GameState]
--allMoves g = map (movePiece g) $ concatMap (pieceMoves (board g) . first intToCoord) squares
--  where
--    squares
--      | whiteTurn g = IM.assocs $ IM.filter whitePiece $ board g
--      | otherwise = IM.assocs $ IM.filter blackPiece $ board g

allMoves :: GameState -> M.Map Coord [Coord]
allMoves g = foldl' buildMap M.empty squares
  where
    buildMap acc s@(x,_)
      | null $ pieceMoves' (board g) s = acc
      | otherwise = M.insert x (pieceMoves' (board g) s) acc
    pType = if whiteTurn g then whitePiece else blackPiece
    squares = map (first intToCoord) $ IM.assocs $ IM.filter pType $ board g

whiteCoords :: Board -> [Coord]
whiteCoords = map intToCoord . IM.keys . IM.filter whitePiece

blackCoords :: Board -> [Coord]
blackCoords = map intToCoord . IM.keys . IM.filter blackPiece

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

------------------------------------------------------------
-- Turn Stuff
------------------------------------------------------------

movePiece :: (Coord,Coord) -> TurnT Square
movePiece (c1,c2) = do
    g <- get
    b <- gets board
    let v1 = snd $ fromJust $ getSquare c1 b
    let  v2 = snd $ fromJust $ getSquare c2 b
    let newB = putPieceBatch b [(c2,v1),(c1,v2)]
    put $ g {board=newB, lastMove=((c1,v1),(c2,v1))}
    return (c2,v1)

escapeCheck :: Square -> TurnT Square
escapeCheck s@(c,p)
  | p == King && c `elem` cornerCoords = left Escape
  | otherwise = return s

findCaptures :: Square -> TurnT [Square]
findCaptures s = do
  b <- gets board
  let adjFoes = filter (liftM2 (&&) (foes s) ((/=Corner) . snd) . fst) $ around b s
  return $ concat $ mapMaybe (uncurry (captures b)) adjFoes

processCaptures :: [Square] -> TurnT ()
processCaptures xs
  | King `elem` map snd xs = left KingCapture
  | otherwise = get >>= \g -> put $ g {board = putPieceBatch (board g) $ map ((,Empty) . fst) xs}


switchTurn :: () -> TurnT GameState
switchTurn _ = get >>= (\g -> put $ g {whiteTurn = not $ whiteTurn g}) >> get

nextMoves :: GameState -> TurnT Moves
nextMoves = return . allMoves

helplessCheck :: Moves -> TurnT Moves
helplessCheck m = do
  b <- gets board
  when (M.null m) $ left $ if not (null ( blackCoords b))
                           then NoMoves else NoPieces
  return m

runTurn :: GameState
         -> (Coord, Coord)
         -> (Either WinLose Moves, GameState)
runTurn g mv = doTurnT actions g
  where actions =   movePiece mv
                >>= escapeCheck
                >>= findCaptures
                >>= processCaptures
                >>= switchTurn
                >>= nextMoves
                >>= helplessCheck
