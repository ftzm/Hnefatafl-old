module Engine (startGame) where

import Board
import Moves
import GameState
import Capture
import Turns hiding (WinLose, Escape)

startGame :: GameState
startGame = GameState {_board = startBoard
                      ,_king = (5,5)
                      ,_whiteIsHuman = True
                      ,_blackIsHuman = True
                      ,_whiteTurn = False
                      ,_lastMove = (((5,0),Black),((5,0),Black))
                      ,_whiteLosses = 0
                      ,_blackLosses = 0
                      ,_allMoves = startMoves
                      }

data MoveResult = KingCapture | Escape |  NoPieces | NoMoves | Continue

-- |
applyRules :: Board -> Square -> (Board, MoveResult)
applyRules = undefined

-- |Check if any pieces have been captured and return a board with them
-- removed.
applyCaptures :: Board -> Square -> (Board, MoveResult)
applyCaptures b s = (board, Continue)
  where board = processCaptures b $ findCaptures b s

processCaptures :: Board -> [Square] -> (Board, MoveResult)
processCaptures = deletePieceBatch

-- |Check if the king has escape
checkEscape :: Square -> MoveResult
checkEscape (c, p)
  | p == King && elem c cornerCoords = Escape
  | otherwise = Continue

-- |Check if there are any pieces left, and if there are, check if there are
-- any moves.
checkHelplessCheck :: Board -> MoveResult
checkHelplessCheck = undefined
