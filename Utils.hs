{-# LANGUAGE FlexibleContexts #-}

module Utils
  ( Epsilon
  , isSymmetric
  , vNormalize
  , mNormalize
  , fromDiag
  , fromDecomposition
  , fromSVD
  , fromSVD'
  , frobNorm
  ) where

import Numeric.LinearAlgebra

type Epsilon = Double

isSymmetric :: (Element a, Eq a) => Matrix a -> Bool
isSymmetric a = toLists a == toLists (trans a)

vNormalize :: Vector Double -> Vector Double
vNormalize v = recip (norm2 v) `scale` v

mNormalize :: Matrix Double -> Matrix Double
mNormalize m = recip (det m) `scale` m

fromDiag :: Vector Double -> Matrix Double
fromDiag v = diagRect 0 v (dim v) (dim v)

fromDecomposition :: (Vector Double, Matrix Double) -> Matrix Double
fromDecomposition (bigLambda, bigV) = bigV <> (fromDiag bigLambda) <> (trans bigV)

fromSVD :: (Matrix Double, Vector Double, Matrix Double) -> Matrix Double
fromSVD (bigU, bigS, bigV) = bigU <> (fromDiag bigS) <> (trans bigV)

fromSVD' :: (Matrix Double, Matrix Double, Matrix Double) -> Matrix Double
fromSVD' (bigU, bigS, bigV) = bigU <> bigS <> (trans bigV)

frobNorm :: Matrix Double -> Double
frobNorm m = norm2 . flatten $ m
