{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Numeric.LinearAlgebra
import Prelude as P hiding (compare)
import System.Random
import Test.QuickCheck
import Text.Printf

import Util

compare :: (Eq a, Show a) => a -> a -> Property
compare ans ref = printTestCase message (ref==ans)
  where
    message = unlines [ "*** Expected:", show ref
                      , "*** Received:", show ans ]

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests = [ ("ident/isSymmetric", quickCheck prop_ident_isSymmetric)
        , ("multByIdent/id", quickCheck prop_multByIdMatrix_id)
        , ("powMeth/eigVals_ex1", quickCheck prop_powMeth_eigVals_ex1)
        , ("powMeth/eigVals_ex2", quickCheck prop_powMeth_eigVals_ex2)
        , ("powMeth/eigVals_ex3", quickCheck prop_powMeth_eigVals_ex3)
        ]

-- An identity matrix is symmetric
prop_ident_isSymmetric n = isSymmetric (ident (1 + abs n `mod` 100) :: Matrix Double)

-- A 2D matrix multiplied by the identity matrix is the same as the original
prop_multByIdMatrix_id i j
  = let r = 1 + abs i `mod` 100
        c = 1 + abs j `mod` 100
        seed = i*j
        arr = reshape c $ randomVector seed Uniform (r*c)
    in  (toLists (ident r <> arr) `compare` toLists arr)

-- Power Method Eigenvalues Example 1
-- ==================================
-- The power method applied to an identity matrix of dimension `n` should return an `n`-length list
-- of eigenvalues [1..] within `epsilon`
prop_powMeth_eigVals_ex1 seed
  = let epsilon = 1.25e-6
        n = 1 + abs seed `mod` 50
        arr = ident n
        gen = mkStdGen seed
        (lambda, v) = powerMethod gen epsilon arr
        lambda' = toList lambda
        trueLambda = take n $ repeat 1 :: [Double]
        comparison = zipWith (-) lambda' trueLambda
    in  ( if not $ all (\e -> abs e < (10 * epsilon)) comparison
          then lambda' `compare` trueLambda
          else True `compare` True )

-- Power Method Eigenvalues Example 2
-- ==================================
-- The power method applied to
--
--    [[13,  5]
--    ,[ 2,  4]]
--
-- should return eigenvalues [14, 3] within `epsilon`
prop_powMeth_eigVals_ex2 seed
  = let epsilon = 1.25e-6
        arr = reshape 2 $ fromList [13, 5, 2, 4]
        gen = mkStdGen seed
        (lambda, v) = powerMethod gen epsilon arr
        lambda' = toList lambda
        trueLambda = [14,3] :: [Double]
        comparison = zipWith (-) lambda' trueLambda
    in  ( if not $ all (\e -> abs e < (10 * epsilon)) comparison
          then lambda' `compare` trueLambda
          else True `compare` True )

-- Power Method Eigenvalues Example 3
-- ==================================
-- The power method applied to any arbitrary, symmetric, NxN decomposable matrix should return the
-- eigenvalues of said matrix as marked against the HMatrix implementation `eigSH`, within
-- `epsilon`; i.e., we want to verify that we are nearly as good as HMatrix :-)
prop_powMeth_eigVals_ex3 seed
  = let epsilon = 1.25e-6
        gen = mkStdGen seed
        n = 1 + abs seed `mod` 15
        t = reshape n $ randomVector seed Uniform (n*n)
        arr = t <> (trans t)
        (lambda, v) = powerMethod gen (0.1 * epsilon) arr
        lambda' = toList lambda
        (trueLambda, _) = eigSH arr
        trueLambda' = toList trueLambda
        comparison = zipWith (-) lambda' trueLambda'
    in  ( if not $ all (\e -> abs e < (10 * epsilon)) comparison
          then lambda' `compare` trueLambda'
          else True `compare` True )
