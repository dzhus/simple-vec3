{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-|

'Vec3' class and implementations.

The package provides two different implementations for 'Vec3' type
class, which differ in storage scheme. Benchmarks are included for
both. You most likely want to use 'CVec3' which is based on contiguous
storage scheme and offers the best performance.

-}

module Data.Vec3
    ( Vec3(..)
    , CVec3(..)
    , TVec3(..)
    )

where

import Control.Monad

import Foreign
import Foreign.C.Types

import Data.Vector.Unboxed as VU
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM
import Test.QuickCheck

import Data.Vec3.Class

import Data.Vec3.Tupled


-- | 'Vec3' implementation with 'Data.Vector.Unboxed.Unbox' and
-- 'Data.Vector.Unboxed.Storable' instances based on a single
-- contiguous array storage scheme, suitable for use with
-- "Data.Vector.Unboxed" and "Data.Vector.Storable".
--
-- @
-- interface: [v1 x   y   z  ; v2 x   y   z  ...], length = N = M / 3
--                |   |   |       |   |   |
-- storage:   [  v1x v2y v2z ;   v2x v2y v2z ...], length = M
-- @
--
-- This implementation has the best performance.
data CVec3 = CVec3 !Double !Double !Double
              deriving (Eq, Show)


instance Vec3 CVec3 where
    newtype Matrix CVec3 = UMatrix (CVec3, CVec3, CVec3)

    fromXYZ (x, y, z) = CVec3 x y z
    {-# INLINE fromXYZ #-}

    toXYZ (CVec3 x y z) = (x, y, z)
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = UMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (UMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}


newtype instance VU.MVector s CVec3 = MV_CVec3 (VU.MVector s Double)
newtype instance VU.Vector    CVec3 = V_CVec3  (VU.Vector    Double)


instance VGM.MVector VU.MVector CVec3 where
    basicInitialize (MV_CVec3 v) =
        VGM.basicInitialize v
    {-# INLINE basicInitialize #-}

    basicLength (MV_CVec3 v) =
        VGM.basicLength v `quot` 3
    {-# INLINE basicLength #-}

    basicUnsafeSlice s l (MV_CVec3 v) =
        MV_CVec3 $ VGM.basicUnsafeSlice (s * 3) (l * 3) v
    {-# INLINE basicUnsafeSlice #-}

    basicOverlaps (MV_CVec3 v1) (MV_CVec3 v2) =
        VGM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}

    basicUnsafeNew n =
        MV_CVec3 `liftM` VGM.basicUnsafeNew (n * 3)
    {-# INLINE basicUnsafeNew #-}

    basicUnsafeRead (MV_CVec3 v) i = do
        x <- VGM.basicUnsafeRead v  j
        y <- VGM.basicUnsafeRead v (j + 1)
        z <- VGM.basicUnsafeRead v (j + 2)
        return $ CVec3 x y z
        where
          j = i * 3
    {-# INLINE basicUnsafeRead #-}

    basicUnsafeWrite (MV_CVec3 v) i (CVec3 x y z) =
        VGM.basicUnsafeWrite v  j      x >>
        VGM.basicUnsafeWrite v (j + 1) y >>
        VGM.basicUnsafeWrite v (j + 2) z
        where
          j = i * 3
    {-# INLINE basicUnsafeWrite #-}


instance VG.Vector VU.Vector CVec3 where
    basicUnsafeFreeze (MV_CVec3 v) =
        V_CVec3 `liftM` VG.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}

    basicUnsafeThaw (V_CVec3 v) =
        MV_CVec3 `liftM` VG.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}

    basicLength (V_CVec3 v) = VG.basicLength v `quot` 3
    {-# INLINE basicLength #-}

    basicUnsafeSlice s l (V_CVec3 v) =
        V_CVec3 $ VG.basicUnsafeSlice (s * 3) (l * 3) v
    {-# INLINE basicUnsafeSlice #-}

    basicUnsafeIndexM (V_CVec3 v) i = do
        x <- VG.basicUnsafeIndexM v  j
        y <- VG.basicUnsafeIndexM v (j + 1)
        z <- VG.basicUnsafeIndexM v (j + 2)
        return $ CVec3 x y z
        where
          j = i * 3
    {-# INLINE basicUnsafeIndexM #-}

    basicUnsafeCopy (MV_CVec3 mv) (V_CVec3 v)
        = VG.basicUnsafeCopy mv v
    {-# INLINE basicUnsafeCopy #-}


instance Unbox CVec3


instance Storable CVec3 where
  sizeOf _    = sizeOf (undefined :: CDouble) * 3
  alignment _ = alignment (undefined :: CDouble)

  peek p = do
      x <- peekElemOff q 0
      y <- peekElemOff q 1
      z <- peekElemOff q 2
      return $ CVec3 x y z
    where
      q = castPtr p
  {-# INLINE peek #-}

  poke p (CVec3 x y z) = do
      pokeElemOff q 0 x
      pokeElemOff q 1 y
      pokeElemOff q 2 z
    where
      q = castPtr p
  {-# INLINE poke #-}

instance Arbitrary CVec3 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ fromXYZ (x, y, z)

  shrink (CVec3 x y z) =
    Prelude.map fromXYZ $ shrink (x, y, z)
