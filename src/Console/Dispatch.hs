module Console.Dispatch (dispatch) where

import           Data.List

import           Console.Runner (runGame)
import           Console.AppState (PlayerType(..), GameOptions(..))
import           Options.Applicative
import           AI.Dispatch

data Command
  = Play GameOptions
  | Benchmark

aiReader :: ReadM PlayerType
aiReader = AI <$> auto

gameOpts :: Parser GameOptions
gameOpts =
  GameOptions
    <$> option aiReader (long "black" <> value Human <> aiHelp)
    <*> option aiReader (long "white" <> value Human <> aiHelp)
    <*> switch (long "record-game")
  where
    aiHelp = help
      ("Specify Mode. Accepts: " ++ (intercalate ", " $ map show allAITypes))

parseCommand :: Parser Command
parseCommand =
  let mkCommand (name, description, parser) =
        command name (info parser (progDesc description))
      commands = foldMap
        mkCommand
        [ ("play" , "Play the game (default)", (Play <$> gameOpts))
        , ("bench", "Run a benchmark"        , (pure Benchmark))
        ]
      fallback = Play <$> gameOpts
  in  hsubparser commands <|> fallback

runCLI :: IO Command
runCLI = execParser $ info' parseCommand "Hnefatafl: The Old-Norse Board Game"
 where
  info' :: Parser a -> String -> ParserInfo a
  info' p desc = info (helper <*> p) (fullDesc <> progDesc desc)

dispatch :: IO ()
dispatch = do
  game <- runCLI
  case game of
    Play opts -> runGame opts
    Benchmark -> putStrLn "bench"
