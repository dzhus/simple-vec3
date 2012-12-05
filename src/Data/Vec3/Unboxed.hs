{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vec3.Unboxed
    ( UVec3(..)
    )

where

import Prelude hiding (reverse)

import Control.Monad

import Data.Vector.Unboxed as VU
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM

import Data.Vec3.Class


-- | 'Vec3' implementation with 'Data.Vector.Unboxed.Unbox' instance
-- based on a single contiguous array storage scheme, suitable for use
-- with "Data.Vector.Unboxed".
--
-- 'Unbox' instance provides the required index transformations.
--
-- @
-- interface: [d1 x   y   z  ; d2 x   y   z  ...], length = N = M / 3
--                |   |   |       |   |   |
-- storage:   [  d1x d2y d2z ;   d2x d2y d2z ...], length = M
-- @
--
-- Thanks to dense packing scheme the performance of this
-- implementation should generally be on par with 'Storable'-based
-- 'SVec'.
data UVec3 = UVec3 !Double !Double !Double
              deriving (Eq, Show)


instance Vec3 UVec3 where
    newtype Matrix UVec3 = UMatrix (UVec3, UVec3, UVec3)

    fromXYZ (x, y, z) = UVec3 x y z
    {-# INLINE fromXYZ #-}

    toXYZ (UVec3 x y z) = (x, y, z)
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = UMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (UMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}


newtype instance VU.MVector s UVec3 = MV_UVec3 (VU.MVector s Double)
newtype instance VU.Vector    UVec3 = V_UVec3  (VU.Vector    Double)


instance VGM.MVector VU.MVector UVec3 where
    basicLength (MV_UVec3 v) =
        VGM.basicLength v `quot` 3
    {-# INLINE basicLength #-}

    basicUnsafeSlice s l (MV_UVec3 v) =
        MV_UVec3 $ VGM.basicUnsafeSlice (s * 3) (l * 3) v
    {-# INLINE basicUnsafeSlice #-}

    basicOverlaps (MV_UVec3 v1) (MV_UVec3 v2) =
        VGM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}

    basicUnsafeNew n =
        MV_UVec3 `liftM` VGM.basicUnsafeNew (n * 3)
    {-# INLINE basicUnsafeNew #-}

    basicUnsafeRead (MV_UVec3 v) i = do
        x <- VGM.basicUnsafeRead v  j
        y <- VGM.basicUnsafeRead v (j + 1)
        z <- VGM.basicUnsafeRead v (j + 2)
        return $ UVec3 x y z
        where
          j = i * 3
    {-# INLINE basicUnsafeRead #-}

    basicUnsafeWrite (MV_UVec3 v) i (UVec3 x y z) =
        VGM.basicUnsafeWrite v  j      x >>
        VGM.basicUnsafeWrite v (j + 1) y >>
        VGM.basicUnsafeWrite v (j + 2) z
        where
          j = i * 3
    {-# INLINE basicUnsafeWrite #-}


instance VG.Vector VU.Vector UVec3 where
    basicUnsafeFreeze (MV_UVec3 v) =
        V_UVec3 `liftM` VG.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}

    basicUnsafeThaw (V_UVec3 v) =
        MV_UVec3 `liftM` VG.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}

    basicLength (V_UVec3 v) = VG.basicLength v `quot` 3
    {-# INLINE basicLength #-}

    basicUnsafeSlice s l (V_UVec3 v) =
        V_UVec3 $ VG.basicUnsafeSlice (s * 3) (l * 3) v
    {-# INLINE basicUnsafeSlice #-}

    basicUnsafeIndexM (V_UVec3 v) i = do
        x <- VG.basicUnsafeIndexM v  j
        y <- VG.basicUnsafeIndexM v (j + 1)
        z <- VG.basicUnsafeIndexM v (j + 2)
        return $ UVec3 x y z
        where
          j = i * 3
    {-# INLINE basicUnsafeIndexM #-}

    basicUnsafeCopy (MV_UVec3 mv) (V_UVec3 v)
        = VG.basicUnsafeCopy mv v
    {-# INLINE basicUnsafeCopy #-}

instance Unbox UVec3
