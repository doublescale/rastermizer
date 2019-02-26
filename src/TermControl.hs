module TermControl
  ( readMouse
  , putAt
  , withBgFgCol
  , clearScreen
  , enableMouse
  , disableMouse
  , hideCursor
  , showCursor
  , getTermSize
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Foldable
import Data.List
import Linear
import System.IO

readMouse :: IO (Int, V2 Int)
readMouse = do
  code <- getUpto (`elem` ['m', 'M'])
  let [status, col, row] = parseCsiNumbers code
  pure (status, V2 col row)

-- TODO: Make this nicer
parseCsiNumbers :: String -> [Int]
parseCsiNumbers = map read
                . filter (not . null)
                . map (filter isDigit)
                . groupBy (\_ y -> isDigit y)

putAt :: V2 Int -> String -> IO ()
putAt p s = moveCursor p *> putStr s

withBgFgCol :: V3 Double -> V3 Double -> String -> String
withBgFgCol bg fg x =
  concat ["\ESC[48;2;", showRGB bg, ";38;2;", showRGB fg, "m", x, "\ESC[0m"]
  where
    showRGB = intercalate ";" . toList
              . fmap (show @Int . max 0 . min 255 . round . (* 255))

moveCursor :: V2 Int -> IO ()
moveCursor (V2 col row) = csi (show row ++ ";" ++ show col ++ "H")

csi :: String -> IO ()
csi x = putStr ("\ESC[" ++ x)

clearScreen :: IO ()
clearScreen = csi "2J"

enableMouse, disableMouse :: IO ()
enableMouse = csi "?1002h" >> csi "?1006h"
disableMouse = csi "?1006l" >> csi "?1002l"

hideCursor, showCursor :: IO ()
hideCursor = csi "?25l"
showCursor = csi "?25h"

getUpto :: (Char -> Bool) -> IO [Char]
getUpto p = do
  [c] <- BS.unpack <$> BS.hGet stdin 1
  rest <- if p c then pure [] else getUpto p
  pure (c : rest)

getCursorPos :: IO (V2 Int)
getCursorPos = do
  csi "6n"
  hFlush stdout
  code <- getUpto (== 'R')
  let [row, col] = parseCsiNumbers code
  pure (V2 col row)

getTermSize :: IO (V2 Int)
getTermSize = do
  moveCursor 999
  getCursorPos
