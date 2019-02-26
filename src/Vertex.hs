{-# LANGUAGE TemplateHaskell #-}

module Vertex where

import Linear

import Control.Lens.Extended

data Vertex a = Vertex
  { position :: V3 a
  , normal :: V3 a
  , color :: V3 a
  } deriving (Functor, Show)

makeLensesWith suffixedLRule ''Vertex

instance Applicative Vertex where
  pure x = Vertex (pure x) (pure x) (pure x)
  (Vertex pf nf cf) <*> (Vertex p n c) = Vertex (pf <*> p) (nf <*> n) (cf <*> c)

instance Additive Vertex where
  zero = pure 0
