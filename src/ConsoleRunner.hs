module ConsoleRunner where

import Engine
import PrintBoard
import Control.Concurrent

import Control.Monad.Trans.Maybe
import Data.Maybe

selectMove :: GameState -> IO GameState
selectMove g = do
  let b = board g
  let ft = frontTurn g
  let coords = if ft then whiteCoords b else blackCoords b
  displayboard b
  piece' <- runMaybeT $ selectByKey coords b
  case piece' of
    Nothing -> putStrLn "Invalid choice" >> threadDelay 1000000 >> selectMove g
    Just piece -> do
      let square = fromJust $ getSquare piece b
      move' <- runMaybeT $ selectByKey (map snd $ pieceMoves b square) b
      case move' of
        Nothing -> selectMove g
        Just move -> return $ movePiece g (piece,move)

generateMove :: GameState -> GameState
generateMove g = bestMove 1 g

gameLoop :: IO GameState -> IO GameState
gameLoop g' = do
  g <- g'
  let human = if frontTurn g then whiteIsHuman g else blackIsHuman g
  newGameState <- if human
     then do
        selectMove g
     else do
        return $ generateMove g
  if (ratio newGameState == -100) || (ratio newGameState == 100)
     then putStrLn "Victory" >> return newGameState
     else gameLoop $ return newGameState

runGameLoop :: IO GameState
runGameLoop = gameLoop $ return startGame
