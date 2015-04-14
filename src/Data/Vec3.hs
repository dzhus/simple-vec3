{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Foreign
import Foreign.C.Types

import Data.Vec3.Class
import Data.Vec3.Unboxed

import Test.QuickCheck


-- | 'Vec3' implementation with 'Foreign.Storable.Storable' instance
-- based on a single contiguous array storage scheme, suitable for use
-- with both "Data.Vector.Storable".
--
-- 'Unbox' instance provides the required index transformations.
--
-- @
-- interface: [d1 x   y   z  ; d2 x   y   z  ...], length = N = M / 3
--                |   |   |       |   |   |
-- storage:   [  d1x d2y d2z ;   d2x d2y d2z ...], length = M
-- @
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


instance Vec3 SVec3 where
    newtype Matrix SVec3 = SMatrix (SVec3, SVec3, SVec3)
                           deriving (Eq, Show)

    fromXYZ (x, y, z) = SVec3 (CDouble x) (CDouble y) (CDouble z)
    {-# INLINE fromXYZ #-}

    toXYZ (SVec3 (CDouble x) (CDouble y) (CDouble z)) = (x, y, z)
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = SMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (SMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}


instance Arbitrary SVec3 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ fromXYZ (x, y, z)

  shrink (SVec3 (CDouble x) (CDouble y) (CDouble z)) =
    map fromXYZ $ shrink (x, y, z)
