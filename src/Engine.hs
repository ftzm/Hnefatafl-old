module Engine where

import Board
import Moves
import GameState

startMovesWhite :: Moves
startMovesWhite = allMovesSplit $ startGame {whiteTurn = True}

startMovesBlack :: Moves
startMovesBlack = allMovesSplit startGame

startGame :: GameState
startGame = GameState {board = startBoard
                      ,king = (5,5)
                      ,whiteIsHuman = True
                      ,blackIsHuman = True
                      , whiteTurn = False
                      ,lastMove = (((5,0),Black),((5,0),Black))
                      ,whiteLosses = 0
                      ,blackLosses = 0
                      ,whiteMoves = startMovesWhite
                      ,blackMoves = startMovesBlack
                      }
