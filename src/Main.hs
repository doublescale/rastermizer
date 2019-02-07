-- TODO: use terminal size to configure projection
-- TODO: load OBJ
-- TODO: use Unicode blocks for drawing, full RGB color
--       consider Map, mapKeys for collapsing two pixel rows
-- TODO: fill triangles

{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Linear
import System.IO

idQuat :: Quaternion Double
idQuat = Quaternion 1 0

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  bracket_ enableMouse disableMouse $
    bracket_ hideCursor showCursor $ do
      velVar <- newMVar idQuat
      _ <- forkIO (mouseHandler velVar)
      animateCube velVar

mouseHandler :: MVar (Quaternion Double) -> IO ()
mouseHandler velVar = go Nothing
  where
    go :: Maybe (V2 Int) -> IO ()
    go dragState = do
      (status, pos) <- readMouse
      let dragging = status == 32
      forM_ (guard dragging *> dragState) $ \lastPos ->
         modifyMVar_ velVar (\v -> pure (posDiffToVel (pos - lastPos) * v))
      go (Just pos)
    posDiffToVel (fmap fromIntegral -> V2 dx dy) =
      axisAngle (unit _y) ( 0.01 * dx) *
      axisAngle (unit _x) (-0.02 * dy)

readMouse :: IO (Int, V2 Int)
readMouse = do
  code <- getUpto (`elem` ['m', 'M'])
  let [status, col, row] = map read
                           . filter (not . null)
                           . map (filter isDigit)
                           . groupBy (\_ y -> isDigit y)
                           $ code
  pure (status, V2 col row)

animateCube :: MVar (Quaternion Double) -> IO ()
animateCube velVar = go idQuat
  where
    go rotState = do
      newRot <- modifyMVar velVar (\v -> pure (dampen v, v * rotState))
      putRotatedCube newRot
      threadDelay 3e4
      go newRot
    dampen v = pow v 0.8

putAt :: String -> V2 Int -> IO ()
putAt s (V2 col row) = csi (show row ++ ";" ++ show col ++ "H" ++ s)

traceLine :: (Additive v, Foldable v) => v Double -> v Double -> [v Double]
traceLine s e =
  take (round (n + 1)) . iterate (^+^ d) $ s
  where
    se = e ^-^ s
    n = maximum (fmap abs se)
    d = se ^/ n

traceEdges :: (Additive v, Foldable v) => [(v Double, v Double)] -> [v Double]
traceEdges = concatMap (uncurry traceLine)

showPoint :: V3 Double -> (String, V2 Int)
showPoint v = ([symbol], round <$> view _xy v)
  where
    depth = view _z v
    symbol | depth <  0.1 = '#'
           | depth <  0.5 = 'X'
           | depth <  0.7 = 'x'
           | otherwise    = '.'

putRotatedCube :: Quaternion Double -> IO ()
putRotatedCube rotation = do
  clearScreen
  mapM_ (uncurry putAt . showPoint)
    . sortOn (negate . view _z)
    . traceEdges
    . over (each . both)
      ( screenTrafo
        . perspProject
        . subtract (3 *^ unit _z)
        . rotate rotation
      )
    $ cubeEdges
  hFlush stdout

-- (-1,-1,-1) to (1,1,1)
cubeEdges :: Num a => [(V3 a, V3 a)]
cubeEdges =
  over (each . both) (over each fromIntegral . subtract 1 . (* 2))
  . filter (\(a, b) -> let d = b-a in minimum d == 0 && sum d == 1)
  . join (liftA2 (,))
  $ liftA3 V3 [0,1] [0,1] [0,1::Int]

perspMat :: M44 Double
perspMat = perspective (pi/2) 1 1 5

perspProject :: V3 Double -> V3 Double
perspProject v = normalizePoint (perspMat !* point v)

screenTrafo :: R2 v => v Double -> v Double
screenTrafo = over _xy $ \v -> (v + V2 1 1) * V2 2 1 ^* 30

csi :: String -> IO ()
csi x = putStr ("\ESC[" ++ x)

clearScreen, enableMouse, disableMouse, hideCursor, showCursor :: IO ()

clearScreen = csi "2J"

enableMouse = csi "?1002h" >> csi "?1006h"
disableMouse = csi "?1006l" >> csi "?1002l"

hideCursor = csi "?25l"
showCursor = csi "?25h"

getUpto :: (Char -> Bool) -> IO [Char]
getUpto p = do
  [c] <- BS.unpack <$> BS.hGet stdin 1
  rest <- if p c then pure [] else getUpto p
  pure (c : rest)
