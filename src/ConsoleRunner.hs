module ConsoleRunner where

import Engine
import PrintBoard
import BasicAI


import Control.Concurrent
import Control.Monad.Trans.Maybe
import Data.Maybe
--import System.IO

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

selectMove :: GameState -> IO GameState
selectMove g = do
  let b = board g
  let ft = frontTurn g
  let coords = if ft then whiteCoords b else blackCoords b
  displayboard b >> pauseUntilKeypress
  piece' <- runMaybeT $ selectByKey coords b
  case piece' of
    Nothing -> putStrLn "Invalid choice" >> threadDelay 1000000 >> selectMove g
    Just piece -> do
      let square = fromJust $ getSquare piece b
      move' <- runMaybeT $ selectByKey (map snd $ pieceMoves b square) b
      case move' of
        Nothing -> selectMove g
        Just move -> return $ movePiece g (piece,move)

makeAIMove :: GameState -> IO GameState
makeAIMove g = displayboard (board g) >> threadDelay 1000000 >> return ( generateMove g)

gameLoop :: IO GameState -> IO GameState
gameLoop g' = do
  g <- g'
  let human = if frontTurn g then whiteIsHuman g else blackIsHuman g
  newGameState <- (if human then selectMove else makeAIMove) g
  if (ratio newGameState == -100) || (ratio newGameState == 100)
     then putStrLn "Victory" >> return newGameState
     else gameLoop $ return newGameState

runGameLoop :: IO GameState
runGameLoop = gameLoop $ configureGame
