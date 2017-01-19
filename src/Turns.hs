{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Turns
  (PostTurn
  ,WinLose(KingCapture,Escape,NoPieces,NoMoves)
  ,runTurn
  )
where

import Data.List
import Data.Maybe
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as V

import Board
import Moves
import Capture
import GameState

data WinLose = KingCapture | Escape |  NoPieces | NoMoves
  deriving (Eq, Show)

newtype TurnT a = TurnT {runTurnT :: EitherT WinLose (State GameState) a}
  deriving (Applicative, Functor, Monad, MonadState GameState, MonadEither WinLose)

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

type PostTurn = (Either WinLose Moves, GameState)

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

doTurnT :: TurnT a -> GameState -> (Either WinLose a, GameState)
doTurnT t = runState (runEitherT $ runTurnT t)

data TurnState = TurnState
                 { gameState :: GameState
                 , winLose :: Maybe WinLose
                 , move :: (Coord,Coord)
                 , captures :: [Square]
                 , nextSideMoves :: Moves
                 }

movePiece :: (Coord,Coord) -> TurnT Square
movePiece (c1,c2) = do
  g <- get
  b <- gets board
  k <- gets king
  let v1 = snd $ getSquare b c1
  let newB = putPiece c2 v1 (deletePiece (c1,v1) b)
  let k2 = if c1 == k then c2 else k
  put $ g {board=newB, lastMove=((c1,v1),(c2,v1)), king=k2}
  return (c2,v1)

movePiece' :: TurnState -> TurnState
movePiece' t = t { gameState = g {board=newB, lastMove=((c1,p1),(c2,p1)), king=k2}}
  where g = gameState t
        (c1,c2) = move t
        b = board g
        k = king g
        p1 = getPiece b c1
        newB = putPiece c2 p1 (deletePiece (c1,p1) b)
        k2 = if c1 == k then c2 else k

postMoveUpdateMoves :: Square -> TurnT Square
postMoveUpdateMoves x = do
  g <- get
  let (s1,s2) = lastMove g
  let g' = updateMovesDelete g s1
  let g'' = updateMovesAdd g' s2
  put g''
  return x

postMoveUpdateMoves' :: TurnState -> TurnState
postMoveUpdateMoves' t = t { gameState = g''}
  where
    g = gameState t
    (s1,s2) = lastMove g
    g' = updateMovesDelete g s1
    g'' = updateMovesAdd g' s2

postMoveUpdateMoves'' :: TurnState -> TurnState
postMoveUpdateMoves'' t = t { gameState = g'}
  where
    g = gameState t
    g' = updateMoves g

escapeCheck :: Square -> TurnT Square
escapeCheck s@(c,p)
  | p == King && c `elem` cornerCoords = left Escape
  | otherwise = return s

escapeCheck' :: TurnState -> TurnState
escapeCheck' t = if p == King && c `elem` cornerCoords
                then t { winLose = Just Escape}
                else t
  where
    lm = lastMove $ gameState t
    (c,p) = snd lm

findCaptures :: Square -> TurnT [Square]
findCaptures s = do
  b <- gets board
  return $ findCaptures' b s

findCaptures'' :: TurnState -> TurnState
findCaptures'' t = t { captures = findCaptures' (board $ g) (snd $ lastMove g)}
  where g = gameState t

recordCaptures :: [Square] -> TurnT [Square]
recordCaptures ss = do
  g <- get
  let losses = length ss
  put $ if whiteTurn g then g {blackLosses = losses} else g {whiteLosses = losses}
  return ss

recordCaptures' :: TurnState -> TurnState
recordCaptures' t = t { gameState = if whiteTurn g then g {blackLosses = losses} else g {whiteLosses = losses}}
  where
    g = gameState t
    losses = length $ captures t

processCaptures :: [Square] -> TurnT [Square]
processCaptures xs
  | King `elem` map snd xs = left KingCapture
  | otherwise = get >>= (\g -> put $ g {board = deletePieceBatch xs $ board g}) >> return xs

processCaptures' :: TurnState -> TurnState
processCaptures' t = t { gameState = g {board = deletePieceBatch cs b}
                       , winLose = if King `elem` map snd cs
                                   then Just KingCapture
                                   else wl
                       }
  where
    g = gameState t
    b = board g
    cs = captures t
    wl = winLose t

postCaptureUpdateMoves :: [Square] -> TurnT ()
postCaptureUpdateMoves xs = do
  g <- get
  let g' = foldl' updateMovesDelete g xs
  put g'

postCaptureUpdateMoves' :: TurnState -> TurnState
postCaptureUpdateMoves' t = t { gameState = foldl' updateMovesDelete g cs}
  where g = gameState t
        cs = captures t

switchTurn :: () -> TurnT GameState
switchTurn _ = get >>= (\g -> put $ g {whiteTurn = not $ whiteTurn g}) >> get

switchTurn' :: TurnState -> TurnState
switchTurn' t = t { gameState = g { whiteTurn = not $ whiteTurn g}}
  where g = gameState t

nextMoves :: GameState -> TurnT Moves
--nextMoves g = return $ allMovesSplit g
nextMoves g = do
  let wt = whiteTurn g
  if wt then return $ whiteMoves g else return $ blackMoves g

nextMoves' :: TurnState -> TurnState
nextMoves' t = t {nextSideMoves=if wt then wms else bms}
  where
    g = gameState t
    wt = whiteTurn g
    bms = blackMoves g
    wms = whiteMoves g

helplessCheck :: Moves -> TurnT Moves
helplessCheck m = do
  b <- gets board
  when (M.null m) $ left $ if isJust $ V.find (==0) b
                           then NoMoves else NoPieces
  return m

helplessCheck' :: TurnState -> TurnState
helplessCheck' t = t { winLose = if empty
                                 then if isJust $ V.find (==0) b
                                      then Just NoMoves
                                      else Just NoPieces
                                 else wl}
  where
    g = gameState t
    b = board g
    empty = M.null $ nextSideMoves t
    wl = winLose t

runTurn' :: GameState -> (Coord, Coord) -> PostTurn
runTurn' g mv = doTurnT actions g
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

runTurn :: GameState -> (Coord, Coord) -> PostTurn
runTurn g mv = pack $ foldl' (\acc f -> f acc) newTurnState actions
  where
    newTurnState = TurnState { gameState=g
                             , move=mv
                             , winLose=Nothing
                             , captures=[]
                             , nextSideMoves=M.empty
                             }
    actions = [ movePiece'
              , postMoveUpdateMoves'
              , escapeCheck'
              , findCaptures''
              , recordCaptures'
              , processCaptures'
              , postCaptureUpdateMoves'
              , switchTurn'
              , nextMoves'
              , helplessCheck'
              ]
    pack :: TurnState -> PostTurn
    pack TurnState {winLose = Nothing, gameState = g'} = (if whiteTurn g' then Right $ whiteMoves g' else Right $ blackMoves g', g')
    pack TurnState {winLose = Just wl, gameState = g'} = (Left wl, g')
