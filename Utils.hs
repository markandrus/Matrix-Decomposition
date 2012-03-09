{-# LANGUAGE FlexibleContexts #-}

module Utils
  ( isSymmetric
  , vNormalize
  , mNormalize
  ) where

import Numeric.LinearAlgebra

isSymmetric :: (Element a, Eq a) => Matrix a -> Bool
isSymmetric a = toLists a == toLists (trans a)

vNormalize :: Vector Double -> Vector Double
vNormalize v = recip (norm2 v) `scale` v

mNormalize :: Matrix Double -> Matrix Double
mNormalize m = recip (det m) `scale` m
