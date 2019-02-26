module Option
  ( Options(..)
  , Charset(..)
  , getOptions
  ) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char
import qualified Options.Applicative as O
import Text.Read

data Options = Options
  { inputFile :: FilePath
  , supersampleFactor :: Int
  , fovRadians :: Double
  , charset :: Charset
  } deriving (Show)

data Charset
  = Ascii
  | Unicode
  deriving (Show)

optionParser :: O.Parser Options
optionParser = do
  inputFile <-
    O.argument O.str (O.metavar "FILE" <> O.help "Input .ply or .dae file")
  supersampleFactor <-
    O.option (O.eitherReader readPositiveInt)
      (O.long "super" <> O.value 1 <> O.showDefault <> O.metavar "FACTOR" <>
       O.help "Supersampling factor")
  fovRadians <- (/ 180) . (* pi) <$>
    O.option O.auto
      (O.long "fov" <> O.value 90 <> O.showDefault <> O.metavar "DEGREES" <>
       O.help "Field of view")
  charset <-
    O.option (O.eitherReader readCharset)
      (O.long "charset" <> O.value Unicode <>
       O.showDefaultWith (map toLower. show) <>
       O.help "Character set (ascii|unicode)")
  pure Options {..}

readPositiveInt :: String -> Either String Int
readPositiveInt input = do
  n <- first (const info) (readEither input)
  when (n < 1) (Left info)
  pure n
  where info = "must be a positive integer"

readCharset :: String -> Either String Charset
readCharset "ascii" = Right Ascii
readCharset "unicode" = Right Unicode
readCharset _ = Left "must be 'ascii' or 'unicode'"

parserInfo :: O.ParserInfo Options
parserInfo =
  O.info
    (optionParser <**> O.helper)
    (O.fullDesc <> O.header "View a 3D file in the terminal.")

getOptions :: IO Options
getOptions = O.execParser parserInfo
