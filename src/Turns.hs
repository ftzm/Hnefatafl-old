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
  return $ findCaptures' b s

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
  when (M.null m) $ left $ if (isJust $ V.find (==0) b )
                           then NoMoves else NoPieces
  return m

runTurn :: GameState -> (Coord, Coord) -> PostTurn
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
