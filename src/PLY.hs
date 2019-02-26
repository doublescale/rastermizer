{-# LANGUAGE OverloadedStrings #-}

module PLY
  ( parse
  ) where

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import GHC.Word
import Linear

import Vertex

-- TODO: Better error messages
parse :: BS.ByteString -> Either String [V3 (Vertex Double)]
parse = P.parseOnly ply

ply :: P.Parser [V3 (Vertex Double)]
ply = do
  _ <- line "ply"
  _ <- line "format ascii 1.0"
  nVerts <- line ("element vertex " *> P.decimal)
  -- TODO: Handle missing and extra fields
  -- (default to 0)
  _ <- line "property float x"
  _ <- line "property float y"
  _ <- line "property float z"
  _ <- line "property float nx"
  _ <- line "property float ny"
  _ <- line "property float nz"
  _ <- line "property uchar red"
  _ <- line "property uchar green"
  _ <- line "property uchar blue"
  nFaces <- line ("element face " *> P.decimal)
  _ <- line "property list uchar uint vertex_indices"
  _ <- line "end_header"
  vertices <- V.replicateM nVerts (line vertex)
  faces <- replicateM nFaces (line face)
  pure (map (indexTri vertices) faces)

vertex :: P.Parser (Vertex Double)
vertex = do
  position <- v3 (tok P.double)
  normal <- v3 (tok P.double)
  color <- v3 (normalizeWord8 <$> tok P.decimal)
  pure Vertex{..}

normalizeWord8 :: Word8 -> Double
normalizeWord8 x = fromIntegral x / 255

-- TODO: Handle non-triangles
face :: P.Parser (V3 Int)
face = "3" *> v3 (tok P.decimal)

indexTri :: V.Vector a -> V3 Int -> V3 a
indexTri verts idx = fmap (verts V.!) idx

line :: P.Parser a -> P.Parser a
line p = comments *> p <* P.endOfLine

comments :: P.Parser ()
comments = void $ many ("comment" <* P.manyTill P.anyChar P.endOfLine)

tok :: P.Parser a -> P.Parser a
tok p = P.skipSpace *> p

v3 :: P.Parser a -> P.Parser (V3 a)
v3 p = liftA3 V3 p p p
