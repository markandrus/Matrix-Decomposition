module SVD (mySVD) where

import Numeric.LinearAlgebra
import System.Random
import Debug.Trace

import PowerMethod
import Utils

-- |mySVD, given a random number generator, a precision parameter `epsilon`, and a 2D MxN matrix,
--  computes a three-tuple containing the MxM matrix U, the RxR diagonal S (where R=min(M,N)), and
--  the NxN matrix V such that the following holds
--
--      A = U S V^T
--
mySVD :: StdGen -> Epsilon -> Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
mySVD g epsilon arr =
  let aT = trans arr
      aTa = aT <> arr
      -- NOTE: that the eigenvalues of $A^T A$ and $A A^T$ are the same, therefore, we keep track
      --       of just `eigVals`
      (eigVals,bigV) = powerMethod g epsilon aTa
      --
      --    S = [ e_1 ... ... ...
      --        , ... e_2 ... ...
      --        , ... ... ... ...
      --        , ... ... ... e_r ]
      --
      bigS = fromDiag eigVals
      -- bigVT = trans bigV
      -- `S` is a diagonal matrix, therefore
      --
      --    S^{-1} = [ 1/s_1    ...     ...     ...
      --             ,   ...  1/s_2     ...     ...
      --             ,   ...    ...     ...     ...
      --             ,   ...    ...     ...   1/s_R ]
      --        
      bigSInverse = fromDiag $ scaleRecip 1 eigVals
      --
      --    U = A V S^{-1}
      --
      bigU = arr <> bigV <> bigSInverse
  in  (bigU, eigVals, bigV)
