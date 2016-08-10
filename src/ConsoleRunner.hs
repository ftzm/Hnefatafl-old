{-# LANGUAGE TupleSections #-}
module ConsoleRunner where

import Engine
import BoardData
import PrintBoard
import BasicAI

import Control.Concurrent
import Control.Monad.Trans.Maybe
import qualified Data.Map as M

getYesNo :: IO Bool
getYesNo = do
  answer <- withoutBuffering getChar
  putStrLn ""
  case answer of
    'y' -> return True
    'n' -> return False
    _   -> getYesNo

yesNo :: String -> IO Bool
yesNo s = putStrLn ( s ++ " (y/n): ") >> getYesNo

configureGame :: IO GameState
configureGame = do
  whiteAnswer <- yesNo "Is the white team human?"
  if whiteAnswer
     then do
      blackAnswer <- yesNo "Is the black team human?"
      return $ startGame {blackIsHuman=blackAnswer}
     else return $ startGame {whiteIsHuman=False, blackIsHuman=True}

selectMove :: GameState -> Moves -> IO (Coord,Coord)
selectMove g m = do
  let b = board g
  displayboard b >> pauseUntilKeypress
  piece' <- runMaybeT $ selectByKey (M.keys m) b
  case piece' of
    Nothing -> putStrLn "Invalid choice" >> threadDelay 1000000 >> selectMove g m
    Just piece -> do
      move' <- runMaybeT $ selectByKey (m M.! piece) b
      case move' of
        Nothing -> selectMove g m
        Just move -> return (piece,move)

makeAIMove :: GameState -> Moves -> IO (Either WinLose Moves, GameState)
--makeAIMove g m = runTurn g <$> (  displayboard (board g)
--                               >> threadDelay 1000000
 --                              >> selectMove g m)
makeAIMove g m = displayboard (board g) >> threadDelay 1000000 >> return ( generateMove g m)


gameOverMessage :: WinLose -> IO ()
gameOverMessage = putStrLn . message
  where
    message t = case t of
                 Escape -> "The King has escaped!"
                 KingCapture -> "The King has been captured!"
                 NoMoves -> "Black has no moves!"
                 NoPieces -> "Black has no pieces!"

gameLoop :: Either WinLose Moves -> GameState
         -> IO (Either WinLose Moves, GameState)
gameLoop r g = either gameOver makeMove r
  where
    gameOver x = gameOverMessage x >> return (r,g)
    human = if whiteTurn g then whiteIsHuman g else blackIsHuman g
    makeMove x = uncurry gameLoop =<< if human
      then runTurn g <$> selectMove g x
      else makeAIMove g x

runGameLoop :: IO (Either WinLose Moves, GameState)
runGameLoop = gameLoop (Right startMoves) =<< configureGame
