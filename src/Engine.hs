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

--import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import qualified Data.IntSet as S
import Control.Applicative
import Control.Monad
--import Control.Arrow hiding (left)
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

type PostTurn = (Either WinLose Moves, GameState)

type Moves = M.Map Coord [Coord]

startGame :: GameState
startGame = GameState startBoard True True True ((5,Black),(5,Black)) 0 0 --True

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
foes (_,p1) (c2,p2)
  | elem c2 cornerCoords = True
  | p1 == Black && c2 == throne = True
  | whitePiece p1 && (c2,p2) == (throne,Empty) = True
  | whitePiece p1 = blackPiece p2
  | blackPiece p1 = whitePiece p2
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

--swap :: (a,b) -> (b,a)
--swap (x,y) = (y,x)

--startBoard :: Board
--startBoard = IM.fromList (whites' ++ blacks' ++ corners ++ king' )
--  where
--    whites' = zip whiteStart (repeat White)
--    blacks' = zip blackStart (repeat Black)
--    corners = zip cornerCoords (repeat Corner)
--    king' = [(throne,King)]

startBoard :: Board
startBoard = Board {blacks=S.fromList blackStart
                     ,whites=S.fromList whiteStart
                     , king=throne
                     }

--getPiece :: Int -> Board -> Piece
--getPiece = IM.findWithDefault Empty
--
--putPiece :: Coord -> Piece -> Board -> Board
--putPiece = IM.insert

getPiece :: Int -> Board -> Piece
getPiece i b
  | S.member i $ blacks b = Black
  | S.member i $ whites b = White
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

deletePieceBatch :: [Square] -> Board -> Board
deletePieceBatch ss b = foldl' (\acc s -> deletePiece s acc) b ss

--getSquare' :: Coord -> Board -> Square
--getSquare' c m = (c,) . (`getPiece` m) c
--
--go' :: Board -> Direction -> Square -> Maybe Square
--go' b d ((x,y),_) = getSquare (c d) b
--  where c North = (x,y-1)
--        c South = (x,y+1)
--        c East  = (x+1,y)
--        c West  = (x-1,y)

------------------------------------------------------------
-- alt go
------------------------------------------------------------

getSquare :: Board -> Int -> (Int,Piece)
getSquare b i = (i,getPiece i b)

go :: Board -> Direction -> (Int,Piece) -> Maybe (Int,Piece)
go b d (i,_) = getSquare b . a <$> ifMaybe' i t
  where (a,t) | North <- d = (,) (subtract 11) (>=11)
              | South <- d = (,) (+11)         (<=109)
              | East  <- d = (,) (+1)          ((/=10) . (`mod`11))
              | West  <- d = (,) (subtract 1)  ((/=0)  . (`mod`11))

------------------------------------------------------------
------------------------------------------------------------

toEdge :: (Int,Piece) -> Direction -> [Int]
toEdge (i,_) North = take (div i 11) $ tail $ iterate (subtract 11) i
toEdge (i,_) South = takeWhile (<120) $ tail $ iterate (+11) i
toEdge (i,_) East = takeWhile ((/=10) . (`mod`11)) $ tail $ iterate (+1) i
toEdge (i,_) West = takeWhile ((/=0)  . (`mod`11)) $ tail $ iterate (subtract 1) i

--fromEdge :: Square -> Maybe Direction
--fromEdge s = just check if going a direction produces a nothing

fromEdge :: Square -> Maybe Direction
fromEdge (x,_)
  | x < 11 = Just South
  | x > 109 = Just North
  | mod x 11 == 0 = Just East
  | mod x 11 == 10 = Just West
  | otherwise = Nothing


--toEdge :: Board -> Square -> Direction -> [Square]
--toEdge b s d = tail . catMaybes . takeWhile isJust
--            $ iterate (go b d =<<) (Just s)



--allMoves :: GameState -> [GameState]
--allMoves g = map (movePiece g) $ concatMap (pieceMoves (board g) . first intToCoord) squares
--  where
--    squares
--      | whiteTurn g = IM.assocs $ IM.filter whitePiece $ board g
--      | otherwise = IM.assocs $ IM.filter blackPiece $ board g

pieceMoves' :: Board -> Square -> [Coord]
pieceMoves' b s@(_,p) = concatMap (takeWhile (eligible . snd . getSquare b) . toEdge s) dirs
  where eligible x | p /= King = x == Empty
                   | p == King = x == Empty || x == Corner

allMoves :: GameState -> M.Map Coord [Coord]
allMoves g = foldl' buildMap M.empty squares
  where
    buildMap acc s@(x,_)
      | null $ pieceMoves' (board g) s = acc
      | otherwise = M.insert x (pieceMoves' (board g) s) acc
    squares = if whiteTurn g
                 then zip (S.toList $ whites $ board g) (repeat White)
                 else zip (S.toList $ blacks $ board g) (repeat Black)

--allMoves :: GameState -> Moves
--allMoves g = foldl' walkRowY (foldl' walkRowX M.empty xBoard) yBoard
--  where
--    b = board g
--    xBoard = map (\x -> map (+x) [0..10]) [0,11..110]
--    yBoard = transpose xBoard
--    walkRowX :: Moves -> [Coord] -> Moves
--    walkRowX m = (\(x,y,z) -> x) . foldl' evalPiece1 (m,[],1000)
--    walkRowY :: Moves -> [Coord] -> Moves
--    walkRowY m = (\(x,y,z) -> x) . foldl' evalPiece2 (m,[],1000)
--    evalPiece1 :: (M.Map Int [Int],[Int],Int) -> Int -> (M.Map Int [Int],[Int],Int)
--    evalPiece1 (m,e,l) i
--      | piece == Empty = collectEmpty
--      | l == 1000 && elem piece friendPieces = (attachForward m,[],i)
--      | l == 1000 = (m,[],1000)
--      | elem piece friendPieces = (attachForward attachBackward,[],i)
--      | otherwise = (attachBackward,[],1000)
--        where
--        piece = getPiece i b
--        collectEmpty = (m,i:e,l)
--        foeEncounter = (m,[],i)
--        attachBackward  = M.insertWith (++) l e m
--        attachForward m = ( M.insert i e m)
--        friendPieces = if whiteTurn g then [White,King] else [Black]
--    evalPiece2 :: (M.Map Int [Int],[Int],Int) -> Int -> (M.Map Int [Int],[Int],Int)
--    evalPiece2 (m,e,l) i
--      | piece == Empty = collectEmpty
--      | l == 1000 && elem piece friendPieces = (attachForward m,[],i)
--      | l == 1000 = (m,[],1000)
--      | elem piece friendPieces = (attachForward attachBackward,[],i)
--      | otherwise = (attachBackward,[],1000)
--        where
--        piece = getPiece i b
--        collectEmpty = (m,i:e,l)
--        foeEncounter = (m,[],i)
--        attachBackward  = M.insertWith (++) l e m
--        attachForward m = ( M.insertWith (++) i e m)
--        friendPieces = if whiteTurn g then [White,King] else [Black]


--whiteCoords :: Board -> [Coord]
--whiteCoords = IM.keys . IM.filter whitePiece
--
--blackCoords :: Board -> [Coord]
--blackCoords = IM.keys . IM.filter blackPiece

ifMaybe :: a -> Bool -> Maybe a
ifMaybe x True = Just x
ifMaybe _ False = Nothing

ifMaybe' :: a -> (a -> Bool) -> Maybe a
ifMaybe' x f | f x = Just x
             | otherwise = Nothing

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
--fromEdge :: Square -> Maybe Direction
--fromEdge ((0,_),_) = Just East
--fromEdge ((10,_),_) = Just West
--fromEdge ((_,0),_) = Just South
--fromEdge ((_,10),_) = Just North
--fromEdge _ = Nothing

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
    where squares = takeWhileIncl (sEq s) $ map (getSquare b) $ toEdge s d

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
    let v1 = snd $ getSquare b c1
    --let newB = putPieceBatch b [(c2,v1),(c1,v2)]
    let newB = putPiece c2 v1 (deletePiece (c1,v1) b)
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
  | otherwise = get >>= \g -> put $ g {board = deletePieceBatch xs $ board g}

recordCaptures :: [Square] -> TurnT [Square]
recordCaptures ss = do
  g <- get
  let losses = length ss
  put $ if whiteTurn g then g {blackLosses = losses} else g {whiteLosses = losses}
  return ss

switchTurn :: () -> TurnT GameState
switchTurn _ = get >>= (\g -> put $ g {whiteTurn = not $ whiteTurn g}) >> get

nextMoves :: GameState -> TurnT Moves
nextMoves = return . allMoves

helplessCheck :: Moves -> TurnT Moves
helplessCheck m = do
  b <- gets board
  when (M.null m) $ left $ if not (S.null ( blacks b))
                           then NoMoves else NoPieces
  return m

runTurn :: GameState
         -> (Coord, Coord)
         -> (Either WinLose Moves, GameState)
runTurn g mv = doTurnT actions g
  where actions =   movePiece mv
                >>= escapeCheck
                >>= findCaptures
                >>= recordCaptures
                >>= processCaptures
                >>= switchTurn
                >>= nextMoves
                >>= helplessCheck
