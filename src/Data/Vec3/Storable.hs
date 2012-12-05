{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vec3.Storable
    ( SVec3(..)
    )

where

import Prelude hiding (reverse)

import Foreign
import Foreign.C.Types

--import Data.Vector.Storable as VS

import Data.Vec3.Class


-- | 'Vec3' implementation with 'Foreign.Storable.Storable' instance,
-- suitable for use with "Data.Vector.Storable".
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

    fromXYZ (x, y, z) = SVec3 (CDouble x) (CDouble y) (CDouble z)
    {-# INLINE fromXYZ #-}

    toXYZ (SVec3 (CDouble x) (CDouble y) (CDouble z)) = (x, y, z)
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = SMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (SMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}
