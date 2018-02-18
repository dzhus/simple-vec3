{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vec3.Unboxed
    ( UVec3(..)
    )

where

import Prelude

import Data.Vector.Unboxed.Deriving

import Data.Vec3.Class


-- | 'Vec3' implementation with 'Data.Vector.Unboxed.Unbox' instance
-- based on tuples, suitable for use with "Data.Vector.Unboxed".
--
-- This represents 3-vector as a triple of doubles, using the default
-- Unbox instance for tuples as provided by "Data.Vector.Unboxed",
-- which wraps a vector of tuples as a tuple of vectors.
--
-- @
-- interface:  [d1 (x, y, z); d2 (x, y, z) ...], length = N
--                  |  |  |       |  |  |
-- storage(x): [d1x-+  |  | ; d2x-+  |  |  ...], length = N
-- storage(y): [d1y----+  | ; d2y----+  |  ...], length = N
-- storage(z): [d1z-------+ ; d2z-------+  ...], length = N
-- @
newtype UVec3 = UVec3 (Double, Double, Double)
                deriving (Eq, Show)


derivingUnbox "UVec3"
  [t|UVec3 -> (Double, Double, Double)|]
  [|\(UVec3 v) -> v|]
  [|UVec3|]


instance Vec3 UVec3 where
    newtype Matrix UVec3 = UMatrix (UVec3, UVec3, UVec3)
                           deriving (Eq, Show)


    fromXYZ = UVec3
    {-# INLINE fromXYZ #-}

    toXYZ (UVec3 v) = v
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = UMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (UMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}


derivingUnbox "UMatrix"
  [t|Matrix UVec3 -> (UVec3, UVec3, UVec3)|]
  [|\(UMatrix v) -> v|]
  [|UMatrix|]
