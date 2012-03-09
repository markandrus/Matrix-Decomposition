module Main where

import Data.List
import Numeric.LinearAlgebra
import System.Random

import Utils
import PowerMethod

main :: IO ()
main = do
  g_0 <- newStdGen
  let (seed,g) = next g_0
      epsilon = 0.000125
      n = 10
      arr_0 = reshape n $ randomVector seed Uniform (n*n)
      arr = arr_0 <> (trans arr_0)
      -- arr = ident n
      -- arr = reshape 3 $ fromList [1, 3, (-3), (-3), 7, (-3), (-6), 6, (-2)]
      (lambda, v) = powerMethod g epsilon (arr)
  putStrLn "powerMethod..."
  print arr
  print lambda
  print v
  putStrLn "eigSH..."
  let (lambda', v') = eigSH arr
  print lambda'
  print v'
