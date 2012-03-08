{-# LANGUAGE TypeOperators #-}

import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Algorithms.Randomish

transpose2D a = backpermute (swap e) swap a
  where
    e = extent a
    swap (Z :. i :. j) = Z :. j :. i

mult a@(Array (Z :. p :. _) _) b@(Array (Z :. q :. _) _) =
    Data.Array.Repa.sum (Data.Array.Repa.zipWith (*) a' b')
  where
    a' = extend (Z :. All :. q   :. All) a
    b' = extend (Z :. p   :. All :. All) $ transpose2D b

mId2D :: DIM2 -> Array DIM2 Float
mId2D sh@(Z :. i :. j) = fromFunction sh (\(Z :. i :. j) -> if i==j then 1 else 0)

main :: IO ()
main = do
  let (p,q,r)=(3,3,3)
  let a = mId2D (Z :. p :. q)
  let b = mId2D (Z :. q :. r)
  putStrLn . show $ mult a b
