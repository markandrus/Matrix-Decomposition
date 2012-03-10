module Main where

import Control.Monad
import Data.List
import Numeric.LinearAlgebra
import System.Environment
import System.IO
import System.Random
import Text.Printf

import Options
import PowerMethod
import SVD
import Utils

main :: IO ()
main = do
  -- Parse options
  opts <- parseOptions =<< getArgs
  let Options { optVerbose = verbose
              , optDecimalPlaces = decimalPlaces
              , optEpsilon = epsilon
              , optDoPowerMethod = doPowerMethod
              , optDoSVD = doSVD
              , optDoRecombine = doRecombine
              , optFilePath = filePath
              } = opts
      -- Setup matrix and vector display functins
      mDisp = putStr . disps decimalPlaces
      vDisp = putStr . vecdisp (disps decimalPlaces)
      -- Create a random number generator
      gen = mkStdGen 777
  -- Read input matrix
  when verbose (printf "Read 2D matrix from `%s'...\n\n\tA = ...\n\n" filePath)
  arr <- loadMatrix filePath
  when verbose (mDisp arr)
  -- Decompose A using the Power Method
  when doPowerMethod (do
      when verbose (hPutStrLn stderr $
          "\n\nComputing eigen-vectors and -values using the Power Method...\n\n"
       ++ "\tA = V \\Lambda V^T")
      -- Compute
      let d@(bigLambda, bigV) = powerMethod gen epsilon arr
      -- Print
      putStrLn "\n# \\Lambda : Vector of eigenvalues"
      vDisp bigLambda
      putStrLn "\n# V : Matrix whose columns are eigenvectors"
      mDisp bigV
      -- Reconstruct A' from the decomposition of A
      when doRecombine (do
          when verbose (hPutStrLn stderr $
              "\n\nComputing the Frobenius norm of the difference between A and "
           ++ "A' = V \\Lambda V^T...")
          -- Compute
          let arr' = fromDecomposition d
          -- Print
          putStrLn "\n# A' : V \\Lambda V^T"
          mDisp arr'
        )
    )
  -- Do the Singular Value Decomposition
  when doSVD (do
      when verbose
        (hPutStrLn stderr $ "\n\nComputing the Singular Value Decomposition...\n\n"
                         ++ "\tA = U S V^T")
      -- Compute
      let d@(bigU, bigS, bigV) = mySVD gen epsilon arr
      -- Print
      putStrLn "\n# U"
      mDisp bigU
      putStrLn "\n# S : Diagonal of singular values"
      vDisp bigS
      putStrLn "\n# V"
      mDisp bigV
      -- Reconstruct A' from the SVD of A
      when doRecombine (do
          when verbose (hPutStrLn stderr
            "\n\nComputing the Frobenius norm of the difference vetween A and A' = U S V^T...")
          -- Compute
          let arr' = fromSVD d
          -- Print
          putStrLn "\n# A' : U S V^T"
          mDisp arr'
        )
    )
