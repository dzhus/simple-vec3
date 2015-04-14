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


-- Access to whole elements
testWhole :: (Show a, Vec3 a, VG.Vector v a) => v a -> v a -> Benchmarkable
testWhole v v2 = (uncurry $ VG.zipWith (<+>)) `whnf` (v, v2)


-- Access to x components of elements
testComp :: (Show a, Vec3 a, VG.Vector v a, VG.Vector v Double) => v a -> v a -> Benchmarkable
testComp v v' = (uncurry $ VG.zipWith foo) `whnf` (v, v')
  where
    foo v1 v2 = x1 + x2
      where
        (x1, _, _) = toXYZ v1
        (x2, _, _) = toXYZ v2


-- zipWith using generic dot product
testDotM :: (Show a, Vec3 a, VG.Vector v a, VG.Vector v Double) => v a -> v a -> Benchmarkable
testDotM v v2 = (uncurry $ VG.zipWith dotM') `whnf` (v, v2)
  where
    o = fromXYZ oXYZ
    m = fromRows (o, o, o)
    dotM' e1 e2 = dotM e1 e2 m


-- Note that source arrays are not forced in test functions.

tv :: VU.Vector UVec3
tv = VG.replicate n $ fromXYZ oXYZ


sv :: VS.Vector SVec3
sv = VG.replicate n $ fromXYZ oXYZ


tv' :: VU.Vector UVec3
tv' = VG.replicate n $ fromXYZ oXYZ'


sv' :: VS.Vector SVec3
sv' = VG.replicate n $ fromXYZ oXYZ'


bsv :: VS.Vector SVec3
bsv = VG.replicate bigN $ fromXYZ oXYZ


btv :: VU.Vector UVec3
btv = VG.replicate bigN $ fromXYZ oXYZ


bsv' :: VS.Vector SVec3
bsv' = VG.replicate bigN $ fromXYZ oXYZ'


btv' :: VU.Vector UVec3
btv' = VG.replicate bigN $ fromXYZ oXYZ'


main :: IO ()
main = defaultMain
       [ bgroup "zipWith"
                    [ bench "SVec/Storable"        $ testWhole sv sv'
                    , bench "UVec/Unboxed"         $ testWhole tv tv'
                    ]
       , bgroup "zipWith-by-x"
                    [ bench "SVec/Storable"        $ testComp sv sv'
                    , bench "UVec/Unboxed"         $ testComp tv tv'
                    ]
       , bgroup "zipWith-dotM"
                    [ bench "SVec/Storable"        $ testDotM sv sv'
                    , bench "UVec/Unboxed"         $ testDotM tv tv'
                    ]
       , bgroup "zipWith 10M"
                    [ bench "SVec/Storable"        $ testWhole bsv bsv'
                    , bench "UVec/Unboxed"         $ testWhole btv btv'
                    ]
       , bgroup "zipWith-by-x 10M"
                    [ bench "SVec/Storable"        $ testComp bsv bsv'
                    , bench "UVec/Unboxed"         $ testComp btv btv'
                    ]
       , bgroup "zipWith-dotM 10M"
                    [ bench "SVec/Storable"        $ testDotM bsv bsv'
                    , bench "UVec/Unboxed"         $ testDotM btv btv'
                    ]
       ]
