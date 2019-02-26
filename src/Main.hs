{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Linear
import System.Exit
import System.IO

import qualified COLLADA
import Option
import qualified PLY
import TermControl
import Vertex

idQuat :: Quaternion Double
idQuat = Quaternion 1 0

main :: IO ()
main = do
  Options {inputFile, supersampleFactor, fovRadians, charset} <- getOptions
  mesh <- readMeshFile inputFile
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  termSize <- getTermSize
  bracket_ enableMouse disableMouse $
    bracket_ hideCursor showCursor $ do
      velVar <- newMVar idQuat
      let pixelRatio =
            case charset of
              Unicode -> 2
              Ascii -> 1
      _ <- forkIO (mouseHandler pixelRatio velVar)
      let renderOptions =
            case charset of
              Unicode ->
                RenderOptions
                  { supersampleFactor
                  , fovRadians
                  , termSize
                  , mapCell = \(V2 _ y) -> if even y then Top else Bottom
                  , pixelRatio
                  , mergeCells = mergeHalfChars
                  , showCell = showHalfChar
                  , mesh
                  }
              Ascii ->
                RenderOptions
                  { supersampleFactor
                  , fovRadians
                  , termSize
                  , mapCell = const id
                  , pixelRatio
                  , mergeCells = const
                  , showCell = showAscii
                  , mesh
                  }
      animateMesh renderOptions velVar

readMeshFile :: FilePath -> IO [V3 (Vertex Double)]
readMeshFile inputFile =
  either showError pure . parse =<< BS.readFile inputFile
  where
    (parse, typeName) =
      if ".dae" `isSuffixOf` map toLower inputFile
      then (COLLADA.parse, "COLLADA")
      else (PLY.parse, "PLY")
    showError s = do
      putStrLn ("Invalid " ++ typeName ++ " format: " ++ inputFile)
      putStrLn s
      exitFailure

mouseHandler :: Int -> MVar (Quaternion Double) -> IO ()
mouseHandler pixelRatio velVar = go Nothing
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
      axisAngle (unit _x) (-0.01 * dy * fromIntegral pixelRatio)

data RenderOptions = forall a. RenderOptions
  { supersampleFactor :: Int
  , fovRadians :: Double
  , termSize :: V2 Int
  , mapCell :: V2 Int -> V3 Double -> a
  , pixelRatio :: Int
  , mergeCells :: a -> a -> a
  , showCell :: a -> String
  , mesh :: [V3 (Vertex Double)]
  }

animateMesh
  :: RenderOptions
  -> MVar (Quaternion Double)
  -> IO ()
animateMesh renderOptions velVar =
  go (axisAngle (unit _z) pi)
  where
    go rotState = do
      newRot <- modifyMVar velVar (\v -> pure (dampen v, v * rotState))
      clearScreen
      mapM_ (uncurry putAt) (rasterizeRotatedMesh renderOptions newRot)
      hFlush stdout
      threadDelay 3e4
      go newRot
    dampen v = pow v 0.8

fillTriangle
  :: Additive v => (v Double -> V2 Double) -> V3 (v Double) -> [v Double]
fillTriangle getCoords attrs = do
  guard frontFacing
  x <- fromIntegral <$> [x0 .. x1]
  y <- fromIntegral <$> [y0 .. y1]
  let p = V2 x y
  guard (intri p)
  pure (bary coords p *! attrs)
  where
    coords@(V3 a b c) = fmap getCoords attrs
    frontFacing = det22 (V2 (b - a) (c - a)) > 0
    x0 :: Int
    V2 (V2 x0 y0) (V2 x1 y1) = fmap round <$> bbox coords
    intri = all (>= 0) . bary coords

bbox :: V3 (V2 Double) -> V2 (V2 Double)
bbox = sequenceA . fmap (liftA2 V2 minimum maximum) . sequenceA

bary :: Fractional a => V3 (V2 a) -> V2 a -> V3 a
bary (V3 (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)) (V2 x y) =
  inv33 (V3 (V3 x1 x2 x3) (V3 y1 y2 y3) 1) !* V3 x y 1

rasterizeRotatedMesh
  :: RenderOptions
  -> Quaternion Double
  -> [(V2 Int, String)]
rasterizeRotatedMesh
  RenderOptions
    { supersampleFactor
    , fovRadians
    , termSize
    , mapCell
    , pixelRatio
    , mergeCells
    , showCell
    , mesh
    }
  rotation
  =
  mesh &
  ( over (each . each . positionL)
      ( rotate rotation
        >>> subtract (3 *^ unit _z)
        >>> perspProject fovRadians renderSize
        >>> screenTrafo pixelRatio renderSize
      )
    >>> filter (allOf (each . positionL . _z) (inRange (-1) 1))
    >>> concatMap (fillTriangle (view (positionL . _xy)))
    >>> map (\v -> (shade (color v) (normal v), position v))
    >>> map (\(col, V3 x y z) -> (round <$> V2 x y, (z, col)))
    >>> Map.fromListWith min
    >>> Map.map snd
    >>> Map.mapKeysWith (+) (fmap (`quot` supersampleFactor))
    >>> Map.map (fmap (/ supersampleSquared))
    >>> Map.mapWithKey mapCell
    >>> Map.mapKeysWith mergeCells (over _y (`div` pixelRatio))
    >>> Map.filterWithKey (\coord _ -> isVisible coord)
    >>> Map.map showCell
    >>> Map.toList
  )
  where
    renderSize = supersampleFactor *^ termSize
    isVisible p = and (liftA2 (>) p (V2 0 0)) && and (liftA2 (<=) p termSize)
    lightDirection = normalize (rotate (pow rotation (-1)) (V3 1 (-3) 2))
    shade col nrm = max 0 (0.2 + 0.8 * (normalize nrm `dot` lightDirection)) *^ col
    supersampleSquared = join (*) (fromIntegral supersampleFactor) :: Double

inRange :: Ord a => a -> a -> a -> Bool
inRange lo hi x = x > lo && x < hi

data HalfChar
  = Top (V3 Double)
  | Bottom (V3 Double)
  | TopBottom (V3 Double) (V3 Double)
  deriving (Show)

mergeHalfChars :: HalfChar -> HalfChar -> HalfChar
mergeHalfChars (Top x) (Bottom y) = TopBottom x y
mergeHalfChars (Bottom x) (Top y) = TopBottom y x
mergeHalfChars _ h                = h

showHalfChar :: HalfChar -> String
showHalfChar (Top tc) = withBgFgCol tc 0 "▄"
showHalfChar (Bottom bc) = withBgFgCol 0 bc "▄"
showHalfChar (TopBottom tc bc) = withBgFgCol tc bc "▄"

showAscii :: V3 Double -> String
showAscii col
  | v >= 0.75 = "#"
  | v >= 0.50 = "X"
  | v >= 0.25 = "x"
  | otherwise = "."
  where
    v = maximum col

perspProject :: Double -> V2 Int -> V3 Double -> V3 Double
perspProject fovRadians (V2 w h) v =
  normalizePoint (perspMat !* point v)
  where
    perspMat :: M44 Double
    perspMat = perspective fovX ratio 0.25 5
    fovX | ratio >= 1 = fovRadians
         | otherwise  = fovRadians * atan (1 / ratio) / atan 1
    ratio = 0.5 * (fromIntegral w / fromIntegral h)

screenTrafo :: R2 v => Int -> V2 Int -> v Double -> v Double
screenTrafo pixelRatio size =
  over _xy $ \v -> (v + V2 1 1)
                   * (fmap fromIntegral size * V2 0.5 yScale)
                   + V2 1 1
  where
    yScale = 0.5 * fromIntegral pixelRatio
