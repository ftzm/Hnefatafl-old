module Engine where

import Board
import Moves
import GameState

startGame :: GameState
startGame = GameState {_board = startBoard
                      ,_king = (5,5)
                      ,_whiteTurn = False
                      ,_lastMove = (((5,0),Black),((5,0),Black))
                      ,_whiteLosses = 0
                      ,_blackLosses = 0
                      ,_allMoves = startMoves
                      }
