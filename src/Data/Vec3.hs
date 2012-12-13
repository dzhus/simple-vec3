{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-|

'Vec3' class and implementations.

-}

module Data.Vec3
    ( Vec3(..)
    , SVec3(..)
    , UVec3(..)
    )

where

import Prelude hiding (reverse)

import Control.Monad

import Foreign
import Foreign.C.Types

import Data.Vector.Unboxed as VU
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM

import Data.Vec3.Class
import Data.Vec3.Unboxed


-- | 'Vec3' implementation with 'Foreign.Storable.Storable' and
-- 'Data.Vector.Unboxed.Unbox' instances based on a single contiguous
-- array storage scheme, suitable for use with both
-- "Data.Vector.Storable" and "Data.Vector.Unboxed".
--
-- 'Unbox' instance provides the required index transformations.
--
-- @
-- interface: [d1 x   y   z  ; d2 x   y   z  ...], length = N = M / 3
--                |   |   |       |   |   |
-- storage:   [  d1x d2y d2z ;   d2x d2y d2z ...], length = M
-- @
--
-- Thanks to dense packing scheme the performance of 'Unbox' instance
-- should generally be on par with 'Storable'.
data SVec3 = SVec3 !CDouble !CDouble !CDouble
             deriving (Eq, Show)


instance Storable SVec3 where
  sizeOf _    = sizeOf (undefined :: CDouble) * 3
  alignment _ = alignment (undefined :: CDouble)

  peek p = do
      x <- peekElemOff q 0
      y <- peekElemOff q 1
      z <- peekElemOff q 2
      return $ SVec3 x y z
    where
      q = castPtr p
  {-# INLINE peek #-}

  poke p (SVec3 x y z) = do
      pokeElemOff q 0 x
      pokeElemOff q 1 y
      pokeElemOff q 2 z
    where
      q = castPtr p
  {-# INLINE poke #-}


newtype instance VU.MVector s SVec3 = MV_SVec3 (VU.MVector s CDouble)
newtype instance VU.Vector    SVec3 = V_SVec3  (VU.Vector    CDouble)


deriving instance VGM.MVector VU.MVector CDouble
deriving instance VG.Vector   VU.Vector  CDouble


instance VGM.MVector VU.MVector SVec3 where
    basicLength (MV_SVec3 v) =
        VGM.basicLength v `quot` 3
    {-# INLINE basicLength #-}

    basicUnsafeSlice s l (MV_SVec3 v) =
        MV_SVec3 $ VGM.basicUnsafeSlice (s * 3) (l * 3) v
    {-# INLINE basicUnsafeSlice #-}

    basicOverlaps (MV_SVec3 v1) (MV_SVec3 v2) =
        VGM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}

    basicUnsafeNew n =
        MV_SVec3 `liftM` VGM.basicUnsafeNew (n * 3)
    {-# INLINE basicUnsafeNew #-}

    basicUnsafeRead (MV_SVec3 v) i = do
        x <- VGM.basicUnsafeRead v  j
        y <- VGM.basicUnsafeRead v (j + 1)
        z <- VGM.basicUnsafeRead v (j + 2)
        return $ SVec3 x y z
        where
          j = i * 3
    {-# INLINE basicUnsafeRead #-}

    basicUnsafeWrite (MV_SVec3 v) i (SVec3 x y z) =
        VGM.basicUnsafeWrite v  j      x >>
        VGM.basicUnsafeWrite v (j + 1) y >>
        VGM.basicUnsafeWrite v (j + 2) z
        where
          j = i * 3
    {-# INLINE basicUnsafeWrite #-}


instance VG.Vector VU.Vector SVec3 where
    basicUnsafeFreeze (MV_SVec3 v) =
        V_SVec3 `liftM` VG.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}

    basicUnsafeThaw (V_SVec3 v) =
        MV_SVec3 `liftM` VG.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}

    basicLength (V_SVec3 v) = VG.basicLength v `quot` 3
    {-# INLINE basicLength #-}

    basicUnsafeSlice s l (V_SVec3 v) =
        V_SVec3 $ VG.basicUnsafeSlice (s * 3) (l * 3) v
    {-# INLINE basicUnsafeSlice #-}

    basicUnsafeIndexM (V_SVec3 v) i = do
        x <- VG.basicUnsafeIndexM v  j
        y <- VG.basicUnsafeIndexM v (j + 1)
        z <- VG.basicUnsafeIndexM v (j + 2)
        return $ SVec3 x y z
        where
          j = i * 3
    {-# INLINE basicUnsafeIndexM #-}

    basicUnsafeCopy (MV_SVec3 mv) (V_SVec3 v)
        = VG.basicUnsafeCopy mv v
    {-# INLINE basicUnsafeCopy #-}


instance Unbox SVec3


instance Vec3 SVec3 where
    newtype Matrix SVec3 = SMatrix (SVec3, SVec3, SVec3)

    fromXYZ (x, y, z) = SVec3 (CDouble x) (CDouble y) (CDouble z)
    {-# INLINE fromXYZ #-}

    toXYZ (SVec3 (CDouble x) (CDouble y) (CDouble z)) = (x, y, z)
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = SMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (SMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}
