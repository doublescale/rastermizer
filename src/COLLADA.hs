-- The supported COLLADA format is very limited.
-- This only supports:
-- * <mesh> with single <triangles> element
-- * <triangles> with exactly semantics VERTEX, NORMAL, COLOR in that order

module COLLADA
  ( parse
  ) where

import Control.Arrow
import Control.Error (headErr, note)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List.Split
import Data.Vector ((!?), Vector)
import qualified Data.Vector as Vector
import Linear
import qualified Text.XML.HXT.Core as HXT

import Vertex

-- TODO: Return XML parsing error in Either
parse :: BS.ByteString -> Either String [V3 (Vertex Double)]
parse = fmap concat . traverse getMesh . getGeometries <=< getTree

getTree :: BS.ByteString -> Either String HXT.XmlTree
getTree =
  headErr "No COLLADA element"
  . HXT.runLA (HXT.xreadDoc >>> HXT.hasName "COLLADA")
  . BS.unpack

getGeometries :: HXT.XmlTree -> [HXT.XmlTree]
getGeometries =
  HXT.runLA (deepName "library_geometries" >>> childName "geometry")

-- TODO: Try Arrow syntax, ArrowApply instead of many runLA
getMesh :: HXT.XmlTree -> Either String [V3 (Vertex Double)]
getMesh xt = do
  -- TODO: Handle different <input> order and count
  posArray <- getSemanticArray "POSITION"
  nrmArray <- getSemanticArray "NORMAL"
  colArray <- getSemanticArray "COLOR"
  let mkVert (V3 pid nid cid) = do
        position <- note "POSITION idx out of bounds" $ posArray !? pid
        normal <- note "NORMAL idx out of bounds" $ nrmArray !? nid
        color <- note "COLOR idx out of bounds" $ colArray !? cid
        pure Vertex{..}
  fmap chunksOfV3 . traverse mkVert $ vertAttrIdxs
  where
    vertAttrIdxs :: [V3 Int]
    vertAttrIdxs =
      chunksOfV3 . map read . concatMap words
      $ HXT.runLA (deepName "triangles" >>> childName "p" >>> getChildText) xt
    sourceIds = flip HXT.runLA xt $
      deepName "input"
      >>> (HXT.getAttrValue0 "semantic"
           &&& (HXT.getAttrValue0 "source" >>> arr (dropWhile (=='#'))))
    getSemanticArray s =
      getFloatArray xt <=< note ("No <input semantic=\"" ++ s ++ "\">")
      $ lookup s sourceIds

getFloatArray :: HXT.XmlTree -> String -> Either String (Vector (V3 Double))
getFloatArray xt i =
  fmap (Vector.fromList . chunksOfV3 . map read . words)
  . headErr ("No float_array with id=\"" ++ i ++ "\"")
  $ HXT.runLA (deepId i >>> childName "float_array" >>> getChildText) xt

deepName :: HXT.ArrowXml a => String -> a HXT.XmlTree HXT.XmlTree
deepName = HXT.deep . HXT.hasName

deepId :: HXT.ArrowXml a => String -> a HXT.XmlTree HXT.XmlTree
deepId = HXT.deep . HXT.hasAttrValue "id" . (==)

childName :: HXT.ArrowXml a => String -> a HXT.XmlTree HXT.XmlTree
childName x = HXT.getChildren >>> HXT.hasName x

getChildText :: HXT.ArrowXml a => a HXT.XmlTree String
getChildText = HXT.getChildren >>> HXT.getText

chunksOfV3 :: [a] -> [V3 a]
chunksOfV3 = map listToV3 . chunksOf 3
  where
    listToV3 [x, y, z] = V3 x y z
    listToV3 a = error $ "Got " ++ show (length a) ++ " instead of 3 elements"
