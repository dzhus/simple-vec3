{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
import Criterion.Main

import Data.Vector.Generic as VG
import Data.Vector.Storable as VS
import Data.Vector.Unboxed as VU

import Data.Vec3


oXYZ :: (Double, Double, Double)
oXYZ = (19.899999999, 22.8, -100500)


oXYZ' :: (Double, Double, Double)
oXYZ' = (2, 12, 85.06)


n :: Int
n = 1000000

bigN :: Int
bigN = n * 10

foo :: Vec3 v => v -> v -> Double
foo v1 v2 = x1 + x2
    where
      (x1, _, _) = toXYZ v1
      (x2, _, _) = toXYZ v2
{-# INLINE foo #-}


-- Access to whole elements
test :: (Show a, Vec3 a, VG.Vector v a) => v a -> v a -> IO ()
test v v2 = do
  let !vr = VG.zipWith (<+>) v v
  return ()
{-# INLINE test #-}


-- Access to x components of elements
testComp :: (Show a, Vec3 a, VG.Vector v a, VG.Vector v Double) => v a -> v a -> IO ()
testComp v v2 = do
  let !vr = VG.zipWith (foo) v v
  return ()
{-# INLINE testComp #-}


-- zipWith using generic dot product
testDotM :: (Show a, Vec3 a, VG.Vector v a, VG.Vector v Double) => v a -> v a -> IO ()
testDotM v v2 = do
  let o = fromXYZ oXYZ
      m = fromRows (o, o, o)
      dotM' e1 e2 = dotM e1 e2 m
      !vr = VG.zipWith dotM' v v2
  return ()
{-# INLINE testDotM #-}


-- Note that source arrays are not forced in test functions.

tv :: VU.Vector TUVec3
tv = VG.replicate n $ fromXYZ oXYZ


uv :: VU.Vector UVec3
uv = VG.replicate n $ fromXYZ oXYZ


sv :: VS.Vector SVec3
sv = VG.replicate n $ fromXYZ oXYZ


tv' :: VU.Vector TUVec3
tv' = VG.replicate n $ fromXYZ oXYZ'


uv' :: VU.Vector UVec3
uv' = VG.replicate n $ fromXYZ oXYZ'


sv' :: VS.Vector SVec3
sv' = VG.replicate n $ fromXYZ oXYZ'


bsv :: VS.Vector SVec3
bsv = VG.replicate bigN $ fromXYZ oXYZ


buv :: VU.Vector UVec3
buv = VG.replicate bigN $ fromXYZ oXYZ


btv :: VU.Vector TUVec3
btv = VG.replicate bigN $ fromXYZ oXYZ


bsv' :: VS.Vector SVec3
bsv' = VG.replicate bigN $ fromXYZ oXYZ'


buv' :: VU.Vector UVec3
buv' = VG.replicate bigN $ fromXYZ oXYZ'


btv' :: VU.Vector TUVec3
btv' = VG.replicate bigN $ fromXYZ oXYZ'


main = defaultMain
       [ bgroup "zipWith"
                    [ bench "Storable"             $ test sv sv'
                    , bench "Unboxed (contiguous)" $ test uv uv'
                    , bench "Unboxed (tuples)"     $ test tv tv'
                    ]
       , bgroup "zipWith-by-x"
                    [ bench "Storable"             $ testComp sv sv'
                    , bench "Unboxed (contiguous)" $ testComp uv uv'
                    , bench "Unboxed (tuples)"     $ testComp tv tv'
                    ]
       , bgroup "zipWith-dotM"
                    [ bench "Storable"             $ testDotM sv sv'
                    , bench "Unboxed (contiguous)" $ testDotM uv uv'
                    , bench "Unboxed (tuples)"     $ testDotM tv tv'
                    ]
       , bgroup "zipWith 10M"
                    [ bench "Storable"             $ test bsv bsv'
                    , bench "Unboxed (contiguous)" $ test buv buv'
                    , bench "Unboxed (tuples)"     $ test btv btv'
                    ]
       , bgroup "zipWith-by-x 10M"
                    [ bench "Storable"             $ testComp bsv bsv'
                    , bench "Unboxed (contiguous)" $ testComp buv buv'
                    , bench "Unboxed (tuples)"     $ testComp btv btv'
                    ]
       , bgroup "zipWith-dotM 10M"
                    [ bench "Storable"             $ testDotM bsv bsv'
                    , bench "Unboxed (contiguous)" $ testDotM buv buv'
                    , bench "Unboxed (tuples)"     $ testDotM btv btv'
                    ]
       ]
