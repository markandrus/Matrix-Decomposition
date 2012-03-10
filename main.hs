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
              , optTestTruncation = testTruncation
              , optFilePath = filePath
              } = opts
      -- Setup matrix and vector display functins
      mDisp = putStr . disps decimalPlaces
      vDisp = putStr . vecdisp (disps decimalPlaces)
      -- Create a random number generator
  gen <- newStdGen
  -- Read input matrix
  when (verbose && (doPowerMethod || doSVD))
    (printf "Read 2D matrix from `%s'...\n\n\tA = ...\n\n" filePath)
  arr <- loadMatrix filePath
  when (verbose && (doPowerMethod || doSVD)) $ mDisp arr
  -- Decompose A using the Power Method
  when doPowerMethod (do
      when verbose (hPutStrLn stderr $
          "\n\nComputing eigen-vectors and -values using the Power Method...\n\n"
       ++ "\tA = V \\Lambda V^T\n")
      -- Compute
      let d@(bigLambda, bigV) = powerMethod gen epsilon arr
      -- Print
      putStrLn "# \\Lambda : Vector of eigenvalues"
      vDisp bigLambda
      putStrLn "\n# V : Matrix whose columns are eigenvectors"
      mDisp bigV
      -- Reconstruct A' from the decomposition of A
      when doRecombine (do
          when verbose (hPutStrLn stderr $
              "\n\nComputing the Frobenius norm of the difference\n"
           ++ "\n\tA - A'  where  A' = V \\Lambda V^T\n")
          -- Compute
          let arr' = fromDecomposition d
          -- Print
          putStrLn "# A' : V \\Lambda V^T"
          mDisp arr'
          let f = frobNorm (arr `sub` arr')
          printf ("\n# | A - A' |\n %." ++ show decimalPlaces ++ "e\n") f
        )
    )
  -- Do the Singular Value Decomposition
  when doSVD (do
      when verbose (hPutStrLn stderr $
          "\n\nComputing the Singular Value Decomposition...\n"
       ++ "\n\tA = U S V^T\n")
      -- Compute
      let d@(bigU, bigS, bigV) = mySVD gen epsilon arr
      -- Print
      putStrLn "# U"
      mDisp bigU
      putStrLn "\n# S : Diagonal of singular values"
      vDisp bigS
      putStrLn "\n# V"
      mDisp bigV
      -- Reconstruct A' from the SVD of A
      when doRecombine (do
          when verbose (hPutStrLn stderr $
              "\n\nComputing the Frobenius norm of the difference\n"
           ++ "\n\tA - A'  where  A' = U S V^T\n")
          -- Compute
          let arr' = fromSVD d
          -- Print
          putStrLn "# A' : U S V^T"
          mDisp arr'
          let f = frobNorm $ arr `sub` arr'
          printf ("\n# | A - A' |\n %." ++ show decimalPlaces ++ "e\n") f
        )
    )
  -- Try reconstructions from truncated values of S
  when testTruncation (do
      -- Make a random 10x20 matrix
      when verbose $ putStrLn "Random 10x20 matrix...\n\n\tA = ...\n"
      let arr = reshape 10 $ randomVector (fst $ next gen) Uniform (10*20)
      -- Verbose print of A
      when verbose $ mDisp arr
      -- Verbose introduction to SVD
      when verbose (hPutStrLn stderr $
          "\n\nComputing the Singular Value Decomposition...\n"
       ++ "\n\tA = U S V^T")
      -- Compute SVD
      let (bigU, bigS, bigV) = mySVD gen epsilon arr
      -- Verbose print of SVD
      when verbose (do
          putStrLn "\n# U"
          mDisp bigU
          putStrLn "\n# S : Diagonal of singular values"
          vDisp bigS
          putStrLn "\n# V"
          mDisp bigV
        )
      -- Verbose introduction to Frobenius norm
      when verbose (hPutStrLn stderr $
          "\n\nComputing the Frobenius norm of the difference between A and reconstructions "
       ++ "from truncated values of S\n")
      -- Compute the norm for truncated values of S
      let truncations = map fromList . tail . inits $ toList bigS
      let n = dim bigS
      putStrLn "# Number of singular values, | A - A' |"
      forM_ truncations (\s -> do
          let bigS' = diagRect 0 s n n
              arr' = fromSVD' (bigU, bigS', bigV)
              f = frobNorm $ arr `sub` arr'
          printf (" %d,\t%." ++ show decimalPlaces ++ "e\n") (dim s) f
        )
    ) 
