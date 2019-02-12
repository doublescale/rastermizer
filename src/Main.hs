-- TODO: full RGB color
-- TODO: fill triangles

{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import qualified Data.Map.Strict as Map
import GHC.Word
import Linear
import System.IO

import TermControl

idQuat :: Quaternion Double
idQuat = Quaternion 1 0

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  termSize <- getTermSize
  bracket_ enableMouse disableMouse $
    bracket_ hideCursor showCursor $ do
      velVar <- newMVar idQuat
      _ <- forkIO (mouseHandler velVar)
      animateCube termSize velVar

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

animateCube :: V2 Int -> MVar (Quaternion Double) -> IO ()
animateCube termSize velVar = go idQuat
  where
    go rotState = do
      newRot <- modifyMVar velVar (\v -> pure (dampen v, v * rotState))
      putRotatedCube termSize newRot
      threadDelay 3e4
      go newRot
    dampen v = pow v 0.8

traceLine :: (Additive v, Foldable v) => v Double -> v Double -> [v Double]
traceLine s e =
  take (round (n + 1)) . iterate (^+^ d) $ s
  where
    se = e ^-^ s
    n = maximum (fmap abs se)
    d = se ^/ (1e-9 + n)

traceEdges :: (Additive v, Foldable v) => [(v Double, v Double)] -> [v Double]
traceEdges = concatMap (uncurry traceLine)

putRotatedCube :: V2 Int -> Quaternion Double -> IO ()
putRotatedCube termSize rotation = do
  clearScreen
  mapM_ (uncurry putAt . (\(p, c) -> (c, p)))
    . filter (isVisible . view _1)
    . mergePoints
    . traceEdges
    . concatMap (\x ->
        over (each . both) (screenTrafo termSize)
        . filter (all (inRange (-1) 1) . toListOf (both . _z))
        . over (each . both)
          ( perspProject termSize
            . subtract (3 *^ unit _z)
            . rotate (pow rotation x)
            . (^* x)
          )
        $ cubeEdges
     ) $ [0, 0.25 .. 1]
  hFlush stdout
  where
    isVisible p = and (liftA2 (>) p (V2 0 0))
                  && and (liftA2 (<=) p termSize)

inRange :: Ord a => a -> a -> a -> Bool
inRange lo hi x = x > lo && x < hi

data HalfChar
  = Top (V3 Word8)
  | Bottom (V3 Word8)
  | TopBottom (V3 Word8) (V3 Word8)
  deriving (Show)

instance Semigroup HalfChar where
  Top x <> Bottom y = TopBottom x y
  Bottom x <> Top y = TopBottom y x
  _ <> h            = h

showHalfChar :: HalfChar -> String
showHalfChar (Top tc) = withBgFgCol tc 0 "▄"
showHalfChar (Bottom bc) = withBgFgCol 0 bc "▄"
showHalfChar (TopBottom tc bc) = withBgFgCol tc bc "▄"

mergePoints :: [V3 Double] -> [(V2 Int, String)]
mergePoints =
  map (\(V3 x y z) -> (fmap round (V2 x y), z))
  >>> Map.fromListWith min
  >>> Map.mapWithKey
      (\(V2 _ y) d -> (if even y then Top else Bottom) (depthCol d))
  >>> Map.mapKeysWith (<>) (over _y (`div` 2))
  >>> Map.toList
  >>> over (each . _2) showHalfChar
  where
    depthCol d = pure $ round (255 * 0.5 * (1 - d))

-- (-1,-1,-1) to (1,1,1)
cubeEdges :: Num a => [(V3 a, V3 a)]
cubeEdges =
  over (each . both) (over each fromIntegral . subtract 1 . (* 2))
  . filter (\(a, b) -> let d = b-a in minimum d == 0 && sum d == 1)
  . join (liftA2 (,))
  $ liftA3 V3 [0,1] [0,1] [0,1::Int]

perspProject :: V2 Int -> V3 Double -> V3 Double
perspProject (V2 w h) v = normalizePoint (perspMat !* point v)
  where
    perspMat :: M44 Double
    perspMat = perspective fovX ratio 1.2 5
    fovX | ratio >= 1 = 2 * atan 1
         | otherwise  = 2 * atan (1 / ratio)
    ratio = 0.5 * fromIntegral w / fromIntegral h

screenTrafo :: R2 v => V2 Int -> v Double -> v Double
screenTrafo termSize =
  over _xy $ \v -> (v + V2 1 1)
                   * (fmap fromIntegral termSize * V2 0.5 1)
                   + V2 1 1
