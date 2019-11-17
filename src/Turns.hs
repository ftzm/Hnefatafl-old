{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TemplateHaskell  #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Turns
  (PostTurn
  ,WinLose(KingCapture,Escape,NoPieces,NoMoves)
  ,runTurn
  )
where

import Data.List
import Data.Maybe
--import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import Control.Lens

import Board
import Moves
import Capture
import GameState

data WinLose = KingCapture | Escape |  NoPieces | NoMoves
  deriving (Eq, Show)

--newtype TurnT a = TurnT {runTurnT :: EitherT WinLose (State GameState) a}
--  deriving (Applicative, Functor, Monad, MonadState GameState, MonadEither WinLose)

--newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}
--
type PostTurn = (Either WinLose Moves, GameState)
--
--instance (Monad m) => Applicative (EitherT e m) where
--  pure = return
--  (<*>) = ap
--
--instance (Monad m) => Functor (EitherT e m) where
--  fmap = liftM
--
--instance MonadTrans (EitherT e) where
--  lift = EitherT . fmap Right
--
--instance (Monad m) => Monad (EitherT e m) where
--  return a = EitherT $ return $ Right a
--  x >>= f = EitherT $ runEitherT x >>= either (return . Left) (runEitherT . f)
--
--instance MonadState s m => MonadState s (EitherT e m) where
--  get = lift get
--  put = lift . put
--
--class MonadEither e m where
--  left :: e -> m a
--
--instance MonadEither e (Either e) where
--  left = Left
--
--instance Monad m => MonadEither e (EitherT e m) where
--  left = EitherT . return . left

--doTurnT :: TurnT a -> GameState -> (Either WinLose a, GameState)
--doTurnT t = runState (runEitherT $ runTurnT t)

data TurnState = TurnState
                 { _gameState :: GameState
                 , _winLose :: Maybe WinLose
                 , _move :: (Coord,Coord)
                 , _captures :: [Square]
                 , _nextSideMoves :: Moves
                 }

makeLenses ''TurnState

--movePiece :: (Coord,Coord) -> TurnT Square
--movePiece (c1,c2) = do
--  g <- get
--  b <- gets board
--  k <- gets king
--  let v1 = snd $ getSquare b c1
--  let newB = putPiece c2 v1 (deletePiece (c1,v1) b)
--  let k2 = if c1 == k then c2 else k
--  put $ g {board=newB, lastMove=((c1,v1),(c2,v1)), king=k2}
--  return (c2,v1)


movePiece' :: TurnState -> TurnState
--movePiece' t = t { gameState = g {board=newB, lastMove=((c1,p1),(c2,p1)), king=k2}}
movePiece' t = t & gameState %~ ((board .~ newB) . (lastMove .~ lm) . (king .~ k2))
  where
    (c1,c2) = t ^. move
    b = t ^. gameState . board
    k = t ^. gameState . king
    p1 = getPiece b c1
    newB = putPiece c2 p1 (deletePiece (c1,p1) b)
    lm = ((c1,p1),(c2,p1))
    k2 = if c1 == k then c2 else k

--postMoveUpdateMoves :: Square -> TurnT Square
--postMoveUpdateMoves x = do
--  g <- get
--  let (s1,s2) = lastMove g
--  let mAx = moveAxis s1 s2
--  let g' = updateMovesDelete g s1 (perp mAx)
--  let g'' = updateMovesAdd g' s2
--  put g''
--  return x

postMoveUpdateMoves' :: TurnState -> TurnState
postMoveUpdateMoves' t = t & gameState . allMoves %~ (postMoveUpdate lt b)
  where
    lt = t ^. gameState . lastMove
    b = t ^. gameState . board


--escapeCheck :: Square -> TurnT Square
--escapeCheck s@(c,p)
--  | p == King && c `elem` cornerCoords = left Escape
--  | otherwise = return s

escapeCheck' :: TurnState -> TurnState
escapeCheck' t = if p == King && c `elem` cornerCoords
                 then t & winLose .~ Just Escape
                 else t
  where
    (c,p) = t ^. gameState . lastMove . _2

--findCaptures :: Square -> TurnT [Square]
--findCaptures s = do
--  b <- gets board
--  return $ findCaptures' b s

findCaptures'' :: TurnState -> TurnState
findCaptures'' t =
  t & captures .~ findCaptures'
  (t ^. gameState . board)
  (t ^. gameState . lastMove . _2)
--findCaptures'' t = t { captures = findCaptures' (board g) (snd $ lastMove g)}
--  where g = gameState t

--recordCaptures :: [Square] -> TurnT [Square]
--recordCaptures ss = do
--  g <- get
--  let losses = length ss
--  put $ if whiteTurn g then g {blackLosses = losses} else g {whiteLosses = losses}
--  return ss

recordCaptures' :: TurnState -> TurnState
recordCaptures' t = t & gameState . k .~ losses
  where
    g = t ^. gameState
    k = if g ^. whiteTurn then blackLosses else whiteLosses
    losses = length $ t ^. captures

--processCaptures :: [Square] -> TurnT [Square]
--processCaptures xs
--  | King `elem` map snd xs = left KingCapture
--  | otherwise = get >>= (\g -> put $ g {board = deletePieceBatch xs $ board g}) >> return xs

processCaptures' :: TurnState -> TurnState
--processCaptures' t = t { gameState = g {board = deletePieceBatch cs b}
--                       , winLose = if King `elem` map snd cs
--                                   then Just KingCapture
--                                   else wl
--                       }
processCaptures' t = t
  & gameState . board %~ (deletePieceBatch (t ^. captures))
  & winLose .~ if King `elem` (map snd $ t ^. captures)
               then Just KingCapture
               else t ^. winLose

--postCaptureUpdateMoves :: [Square] -> TurnT ()
--postCaptureUpdateMoves xs = do
--  g <- get
--  let g' = foldl' (\acc x -> updateMovesDelete g x dirs) g xs
--  put g'

postCaptureUpdateMoves' :: TurnState -> TurnState
postCaptureUpdateMoves' t = t & gameState . allMoves %~ ((flip (postDeleteUpdate (t ^. gameState . board))) (t ^. captures))

--switchTurn :: () -> TurnT GameState
--switchTurn _ = get >>= (\g -> put $ g {whiteTurn = not $ whiteTurn g}) >> get

switchTurn' :: TurnState -> TurnState
switchTurn' = gameState . whiteTurn %~ not


--nextMoves :: GameState -> TurnT Moves
----nextMoves g = return $ allMovesSplit g
--nextMoves g = do
--  let wt = whiteTurn g
--  if wt then return $ whiteMoves g else return $ blackMoves g

nextMoves' :: TurnState -> TurnState
nextMoves' t = t & nextSideMoves .~ if wt then wms else bms
  where
    wt  = t ^. gameState . whiteTurn
    bms = t ^. gameState . allMoves . blackMoves
    wms = t ^. gameState . allMoves . whiteMoves


--helplessCheck :: Moves -> TurnT Moves
--helplessCheck m = do
--  b <- gets board
--  when (M.null m) $ left $ if isJust $ V.find (==0) b
--                           then NoMoves else NoPieces
--  return m

helplessCheck' :: TurnState -> TurnState
helplessCheck' t = t & winLose .~ if empty
                                 then if isJust $ V.find (==Black) b
                                      then Just NoMoves
                                      else Just NoPieces
                                 else wl
  where
    b = t ^. gameState . board
    empty = M.null $ t ^. nextSideMoves
    wl = t ^. winLose

--runTurn' :: GameState -> (Coord, Coord) -> PostTurn
--runTurn' g mv = doTurnT actions g
--  where actions =   movePiece mv
--                >>= postMoveUpdateMoves
--                >>= escapeCheck
--                >>= findCaptures
--                >>= recordCaptures
--                >>= processCaptures
--                >>= postCaptureUpdateMoves
--                >>= switchTurn
--                >>= nextMoves
--                >>= helplessCheck

runTurn :: GameState -> (Coord, Coord) -> PostTurn
runTurn g mv = pack $ foldl' (\acc f -> f acc)
               newTurnState actions
  where
    newTurnState = TurnState
      { _gameState=g
      , _move=mv
      , _winLose=Nothing
      , _captures=[]
      , _nextSideMoves=M.empty
      }
    actions =
      [ movePiece'
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
    pack TurnState {_winLose = Nothing, _gameState = g'}
      = (if g' ^. whiteTurn
          then Right $ g' ^. allMoves . whiteMoves
          else Right $ g' ^. allMoves . blackMoves
        , g'
        )
    pack TurnState {_winLose = Just wl, _gameState = g'}
      = (Left wl, g')
