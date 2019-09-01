module Dispatch (dispatch) where

import           Console.Runner (runGame)
import           Console.AppState (PlayerType(..), GameOptions(..))
import           Options.Applicative

data Command
  = Play GameOptions
  | Benchmark

aiReader :: ReadM PlayerType
aiReader = AI <$> str

gameOpts :: Parser GameOptions
gameOpts =
  GameOptions
    <$> option aiReader (long "black" <> value Human)
    <*> option aiReader (long "white" <> value Human)
    <*> switch (long "record-game")

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
