module Engine where

import Board
import Moves
import GameState

startMovesWhite :: Moves
startMovesWhite = allMovesSplit startGame

startMovesBlack :: Moves
startMovesBlack = allMovesSplit $ startGame {whiteTurn = False}

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
