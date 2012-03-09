module PowerMethod (powerMethod) where

import Numeric.LinearAlgebra
import System.Random
import Debug.Trace

import Utils

-- |powerIterations takes a seed for a random vector and an array, and returns a (hopefully)
--  convergent list of `r` vectors zipped with their residuals using the power method; we want `r`
--  to converge to the first eigenvector of the array
powerIterations
  :: Int -> Matrix Double -> [(Vector Double, Double)]
powerIterations seed arr =
  -- NOTE: that `rows arr == cols arr`
  let r = (fromList . take (rows arr) $ repeat 1) `add` (randomVector seed Uniform $ rows arr)
      -- Here we construct an infinite list of applications of
      --
      --    r \leftarrow \frac{ Ar } { ||Ar|| }
      --
      -- to our initial random vector, `r`.
      iterations = iterate (\r -> vNormalize $ arr <> r) r
      -- Next, we construct an infinite list of `e`'s computed from subtracting pairs of sequential
      -- iterations; `e` should converge to zero and `iterations` to the first eigenvalue in A
      -- NOTE: we set the first value of `e` to `Infinity`
      es = (1/0) : zipWith (\a b -> abs . norm2 $ sub a b) iterations (tail iterations)
  -- Return the zip of the residuals and each computed `r`
  in  zip iterations es

-- |eigVecAndVal takes an Int `i` representing which eigenvalue we are looking for (`i` is used
--  to compute `lambda`), a precision parameter `epsilon`, a random seed for `powerIterations` and
--  an array; NOTE: the array should already have eigenvectors 1 through `i` zeroed out
eigVecAndVal :: Int -> Int -> Double -> Matrix Double -> (Vector Double, Double)
-- NOTE: since we implement no checks as to whether the matrix passed in is actually decomposible,
--       i.e., whether or not `r` will converge to the dominant eigenvector, `eigVecAndVal`
--       occasionally stalls calling `powerIterations`--at which point it may be useful to
--       uncomment the following line to enable visual inspection of the matrix
{-eigVecAndVal seed i epsilon arr
  | trace ("eigVecAndVal " ++ show seed ++ " " ++ show i ++ " " ++ show epsilon ++ " " ++ show arr)
      False = undefined-}
eigVecAndVal seed i epsilon arr =
  let zippedIterations = powerIterations seed arr
      -- Return the first computed `r` for which the residual is less than epsilon; this is approx.
      --
      --    v_i = \text{ the $i$'th normalized eigenvector of $A$ }
      --
      -- NOTE: `v_i` is already normalized
      v_i = fst . head $ dropWhile (\(_,e) -> e >= (0.1 * epsilon)) zippedIterations
      -- `lambda` is the `i`'th eigenvalue, computed as
      --
      --    \lambda_i = \frac{ [Av_i]_i } { [v_i]_i }
      --
      lambda_i = ((arr <> v_i) @> i) / (v_i @> i)
  -- Return a tuple containing the `i`'th eigenvector and eigenvalue
  in  (v_i, lambda_i)

-- |powerMethod takes a gen to compute random vectors, a precision parameter `epsion`, and an NxN
--  matrix and computes the decomposition
--
--      Av_i = V \Lambda V^T = \lambda_i v_i
--
--  powerMethod returns $(V, \Lambda)$
--
--  NOTE: the output resembles that of HMatrix's built-in `eigSH` for easy checking
powerMethod
  :: StdGen -> Double -> Matrix Double -> (Vector Double, Matrix Double)
powerMethod g epsilon arr =
  -- NOTE: that `rows arr == cols arr`
  let is = [1..cols arr]
      -- `step` takes a tuple whose components represent input and output; the input contains an
      -- array we modify over the course of execution (*) and a random number generator which we
      -- use to produce random vectors in `powerIterations`; the output is a list of tuples whose
      -- components are the `n-i`'th eigenvector and eigenvalue, respectively (**)
      step i ((arr,g), vls) =
        -- Get a new random number generator
        let (seed, g') = next g
            -- Calculate the `i`'th eigenvector and eigenvalue
            vl@(v, lambda_i) = eigVecAndVal seed (i-1) epsilon arr
            v_i = reshape 1 v
            -- (*) Ensure that the part of `r` as calculated in step `i+1` will be zeroed out by
            -- updating the array as follows
            --
            --    A \leftarrow A - \lambda_i v_i v_i^T
            --
            -- NOTE: after much troubleshooting and wondering why this algorithm was non-
            --       terminating, it appears the writeup *did not* imply the following, commented-
            --       out definition of `arr'`
            arr' = arr `sub` (lambda_i `scale` (v_i <> trans v_i))
            -- arr' = mapMatrixWithIndex (\(x,y) v -> if (round x)==i || (round y)==i then 0 else v) arr
        -- Return the modified array and append `lambda`
        in  ((arr',g'), vl:vls)
      -- NOTE: (**) requires that we reverse the list
      (vs,ls) = unzip . reverse . snd
              $ foldr step ((arr,g),[]) is
      bigV = fromColumns vs
      bigLambda = fromList ls
  -- Return a tuple containing a matrix, `bigV`, whose columns are the eigenvectors of `arr` and
  -- the diagonal of `bigV`, AKA the eigenvalues of `arr`
  in  (bigLambda, bigV)
