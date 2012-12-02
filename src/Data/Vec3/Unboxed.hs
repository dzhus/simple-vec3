{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-|

Suitable for use with "Data.Vector.Unboxed".

-}

module Data.Vec3.Unboxed
    ( UVec3(..)
    )

where

import Prelude hiding (reverse)

import Data.Vector.Unboxed as VU
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VG

import Data.Vec3.Class

-- | Vector in @R^3@.
newtype UVec3 = UVec3 (Double, Double, Double)
    deriving (Eq, Show,
              VG.Vector VU.Vector,
              VG.MVector VU.MVector,
              VU.Unbox)


instance Vec3 UVec3 where
    newtype Matrix UVec3 = UMatrix (UVec3, UVec3, UVec3)

    origin = UVec3 (0, 0, 0)
    {-# INLINE origin #-}

    fromXYZ v = UVec3 v
    {-# INLINE fromXYZ #-}

    toXYZ (UVec3 v) = v
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = UMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (UMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}

    (<+>) (UVec3 (x1, y1, z1)) (UVec3 (x2, y2, z2)) = 
        UVec3 (x1 + x2, y1 + y2, z1 + z2)
    {-# INLINE (<+>) #-}

    (<->) (UVec3 (x1, y1, z1)) (UVec3 (x2, y2, z2)) = 
        UVec3 (x1 - x2, y1 - y2, z1 - z2)
    {-# INLINE (<->) #-}

    (><) (UVec3 (x1, y1, z1)) (UVec3 (x2, y2, z2)) =
        UVec3 (y1 * z2 - y2 * z1, x2 * z1 - x1 * z2, x1 * y2 - x2 * y1)
    {-# INLINE (><) #-}

    (.^) (UVec3 (x, y, z)) s = 
        UVec3 (x * s, y * s, z * s)
    {-# INLINE (.^) #-}

    (.*) (UVec3 (x1, y1, z1)) (UVec3 (x2, y2, z2)) = 
        x1 * x2 + y1 * y2 + z1 * z2
    {-# INLINE (.*) #-}
