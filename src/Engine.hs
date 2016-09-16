{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import qualified Data.IntSet as S
import Control.Applicative
import Control.Monad
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
    , whiteMoves :: Moves
    , blackMoves :: Moves
    }
  deriving (Show)

type PostTurn = (Either WinLose Moves, GameState)

type Moves = M.Map Coord [[Coord]]

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

startBoard :: Board
startBoard = Board {blacks=S.fromList blackStart
                     ,whites=S.fromList whiteStart
                     , king=throne
                     }

getPiece :: Board -> Int -> Piece
getPiece b i
  | S.member i $ blacks b = Black
  | S.member i $ whites b = White
  | i `elem` cornerCoords = Corner
  | i == king b = King
  | otherwise = Empty

putPiece :: Coord -> Piece -> Board -> Board
putPiece i Black b = b {blacks = S.insert i $ blacks b}
putPiece i White b = b {whites = S.insert i $ whites b}
putPiece i King b = b {king=i}

putPieceBatch :: Board -> [Square] ->  Board
putPieceBatch = foldl' (\b (x,y) -> putPiece x y b)

deletePiece :: Square -> Board -> Board
deletePiece (i,Black) b = b {blacks = S.delete i $ blacks b}
deletePiece (i,White) b = b {whites = S.delete i $ whites b}
deletePiece (_,King) b = b

deletePieceBatch :: [Square] -> Board -> Board
deletePieceBatch ss b = foldl' (flip deletePiece) b ss

getSquare :: Board -> Int -> (Int,Piece)
getSquare b i = (i,getPiece b i)


ifMaybe' :: a -> (a -> Bool) -> Maybe a
ifMaybe' x f | f x = Just x
             | otherwise = Nothing

go :: Board -> (Int,Piece) -> Direction -> Maybe (Int,Piece)
go b (i,_) d = getSquare b . a <$> ifMaybe' i t
  where (a,t) | North <- d = (,) (subtract 11) (>=11)
              | South <- d = (,) (+11)         (<=109)
              | East  <- d = (,) (+1)          ((/=10) . (`mod`11))
              | West  <- d = (,) (subtract 1)  ((/=0)  . (`mod`11))

toEdge :: (Int,Piece) -> Direction -> [Int]
toEdge (i,_) North = take (div i 11) $ tail $ iterate (subtract 11) i
toEdge (i,_) South = take (div (120-i) 11) $ tail $ iterate (+11) i
toEdge (i,_) East  = take (10 - mod i 11) $ tail $ iterate (+1) i
toEdge (i,_) West  = take (mod i 11) $ tail $ iterate (subtract 1) i

fromEdge :: Square -> Maybe Direction
fromEdge (x,_)
  | x < 11 = Just South
  | x > 109 = Just North
  | mod x 11 == 0 = Just East
  | mod x 11 == 10 = Just West
  | otherwise = Nothing

------------------------------------------------------------
--- Dealing with Moves
------------------------------------------------------------

dirMoves :: Board -> Square -> Direction -> [Coord]
dirMoves b s@(_,p) = takeWhile (eligible . snd . getSquare b) . toEdge s
  where eligible x | p == King = x == Empty || x == Corner
                   | otherwise = x == Empty

pieceMovesSplit :: Board -> Square -> [[Coord]]
pieceMovesSplit b s = map (dirMoves b s) dirs

findNextPiece :: Board -> Square -> Direction -> Maybe Square
findNextPiece b s d = find (\(_,p) -> (p /= Empty && p /= Corner)) $ map (getSquare b) $ toEdge s d

modifyWhiteMoves :: GameState -> (Moves -> Moves) -> GameState
modifyWhiteMoves g f = g {whiteMoves = f $ whiteMoves g}

modifyBlackMoves :: GameState -> (Moves -> Moves) -> GameState
modifyBlackMoves g f = g {blackMoves = f $ blackMoves g}

modifyMoves :: Square -> (GameState -> (Moves -> Moves) -> GameState)
modifyMoves (_,p) | p == Black = modifyBlackMoves | otherwise  = modifyWhiteMoves

writeDirMoves :: Square -> Direction -> [Coord] -> Moves -> Moves
writeDirMoves (c,_) d cs m
  | Just existing <- M.lookup c m = M.insert c (f d existing cs) m
  | Nothing <- M.lookup c m = M.insert c (f d [[],[],[],[]] cs) m
  where f d' [h,i,j,k] x
          | North <- d' = [x,i,j,k]
          | East  <- d' = [h,x,j,k]
          | South <- d' = [h,i,x,k]
          | West  <- d' = [h,i,j,x]

adjustDirMoves :: Square -> GameState -> Direction -> GameState
adjustDirMoves s g d = maybe g (write g d) nextPiece
  where
    b = board g
    nextPiece = findNextPiece b s d
    write g' d' s'= modifyMoves s' g' (writeDirMoves s' (opp d') (dirMoves b s' (opp d')))

updateMovesAround :: GameState -> Square -> GameState
updateMovesAround g s = foldl' (adjustDirMoves s) g dirs

deleteMoves :: GameState -> Square -> GameState
deleteMoves g s@(c,_) = modifyMoves s g (M.delete c)

addMoves :: GameState -> Square -> [[Coord]] -> GameState
addMoves g s@(c,_) ms = modifyMoves s g (M.insert c ms)

updateMovesDelete :: GameState -> Square -> GameState
updateMovesDelete g s = updateMovesAround (deleteMoves g s) s

updateMovesAdd :: GameState -> Square -> GameState
updateMovesAdd g s = updateMovesAround (addMoves g s (pieceMovesSplit (board g) s)) s

allMovesSplit :: GameState -> Moves
allMovesSplit g = foldl' buildMap M.empty squares
  where
    buildMap acc s@(x,_)
      | all null $ pieceMovesSplit (board g) s = acc
      | otherwise = M.insert x (pieceMovesSplit (board g) s) acc
    squares = if whiteTurn g
                 then zip (S.toList $ whites $ board g) (repeat White)
                 else zip (S.toList $ blacks $ board g) (repeat Black)

--lastOr :: a -> [a] -> a
--lastOr x [] = x
--lastOr _ (x:xs) = lastOr x xs

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

------------------------------------------------------------
-- Turn Stuff
------------------------------------------------------------

movePiece :: (Coord,Coord) -> TurnT Square
movePiece (c1,c2) = do
    g <- get
    b <- gets board
    let v1 = snd $ getSquare b c1
    let newB = putPiece c2 v1 (deletePiece (c1,v1) b)
    put $ g {board=newB, lastMove=((c1,v1),(c2,v1))}
    return (c2,v1)

postMoveUpdateMoves :: Square -> TurnT Square
postMoveUpdateMoves x = do
  g <- get
  let (s1,s2) = lastMove g
  let g' = updateMovesDelete g s1
  let g'' = updateMovesAdd g' s2
  put g''
  return x

escapeCheck :: Square -> TurnT Square
escapeCheck s@(c,p)
  | p == King && c `elem` cornerCoords = left Escape
  | otherwise = return s

findCaptures :: Square -> TurnT [Square]
findCaptures s = do
  b <- gets board
  let adjFoes = filter (liftM2 (&&) (foes s) ((/=Corner) . snd) . fst) $ around b s
  return $ concat $ mapMaybe (uncurry (captures b)) adjFoes

recordCaptures :: [Square] -> TurnT [Square]
recordCaptures ss = do
  g <- get
  let losses = length ss
  put $ if whiteTurn g then g {blackLosses = losses} else g {whiteLosses = losses}
  return ss

processCaptures :: [Square] -> TurnT [Square]
processCaptures xs
  | King `elem` map snd xs = left KingCapture
  | otherwise = get >>= (\g -> put $ g {board = deletePieceBatch xs $ board g}) >> return xs

postCaptureUpdateMoves :: [Square] -> TurnT ()
postCaptureUpdateMoves xs = do
  g <- get
  let g' = foldl' updateMovesDelete g xs
  put g'

switchTurn :: () -> TurnT GameState
switchTurn _ = get >>= (\g -> put $ g {whiteTurn = not $ whiteTurn g}) >> get

nextMoves :: GameState -> TurnT Moves
--nextMoves g = return $ allMovesSplit g
nextMoves g = do
  let wt = whiteTurn g
  if wt then return $ whiteMoves g else return $ blackMoves g

helplessCheck :: Moves -> TurnT Moves
helplessCheck m = do
  b <- gets board
  when (M.null m) $ left $ if not (S.null ( blacks b))
                           then NoMoves else NoPieces
  return m

runTurn :: GameState -> (Coord, Coord) -> (Either WinLose Moves, GameState)
runTurn g mv = doTurnT actions g
  where actions =   movePiece mv
                >>= postMoveUpdateMoves
                >>= escapeCheck
                >>= findCaptures
                >>= recordCaptures
                >>= processCaptures
                >>= postCaptureUpdateMoves
                >>= switchTurn
                >>= nextMoves
                >>= helplessCheck
