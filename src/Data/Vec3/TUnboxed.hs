{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-|

'Vec3' implementation with 'Data.Vector.Unboxed.Unbox' instance based
on tuples, suitable for use with "Data.Vector.Unboxed".

-}

module Data.Vec3.TUnboxed
    ( TUVec3(..)
    )

where

import Prelude hiding (reverse)

import Data.Vector.Unboxed as VU
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VG

import Data.Vec3.Class


newtype TUVec3 = TUVec3 (Double, Double, Double)
                deriving (Eq, Show,
                          VG.Vector VU.Vector,
                          VG.MVector VU.MVector,
                          VU.Unbox)


instance Vec3 TUVec3 where
    newtype Matrix TUVec3 = TUMatrix (TUVec3, TUVec3, TUVec3)
                           deriving (Eq, Show,
                                     VG.Vector VU.Vector,
                                     VG.MVector VU.MVector,
                                     VU.Unbox)


    fromXYZ v = TUVec3 v
    {-# INLINE fromXYZ #-}

    toXYZ (TUVec3 v) = v
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = TUMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (TUMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}
