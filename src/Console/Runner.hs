module Console.Runner
  ( runGame
  ) where

import           Console.AppState
import           Console.Draw
import           Console.Events

import           Brick
import           Brick.BChan (newBChan, writeBChan, BChan)
import           Control.Concurrent (ThreadId, forkIO, threadDelay)
import           Control.Monad (forever, void)
import           Graphics.Vty (mkVty, defaultConfig)

-------------------------------------------------------------------------------

app :: App AppState AsyncEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attributes
          }

runTick :: BChan AsyncEvent -> IO ThreadId
runTick chan =
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000000 -- decides how frequently the Tick is sent

runGame :: GameOptions -> IO ()
runGame o = do
  chan <- newBChan 10
  runTick chan
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app $ startState chan
