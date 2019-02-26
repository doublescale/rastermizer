module Control.Lens.Extended
  ( module Control.Lens
  , suffixedLRule
  ) where

import Control.Lens

suffixedLRule :: LensRules
suffixedLRule = lensRules & lensField .~ mappingNamer (\x -> [x ++ "L"])
