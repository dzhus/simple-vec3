{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vec3.Tupled
    ( TVec3(..)
    )

where

import Prelude

import Data.Vector.Unboxed.Deriving

import Data.Vec3.Class


-- | 'Vec3' implementation with 'Data.Vector.Unboxed.Unbox' instance
-- based on default Unbox instance for tuples of arrays, which wraps a
-- vector of tuples as a tuple of vectors.
--
-- @
-- interface:  [v1 (x, y, z); v2 (x, y, z) ...], length = N
--                  |  |  |       |  |  |
-- storage(x): [v1x-+  |  | ; v2x-+  |  |  ...], length = N
-- storage(y): [v1y----+  | ; v2y----+  |  ...], length = N
-- storage(z): [v1z-------+ ; v2z-------+  ...], length = N
-- @
--
-- You almost definitely want to use 'CVec3' instead as it has better
-- performance.
newtype TVec3 = TVec3 (Double, Double, Double)
               deriving (Eq, Show)


derivingUnbox "TVec3"
  [t|TVec3 -> (Double, Double, Double)|]
  [|\(TVec3 v) -> v|]
  [|TVec3|]


instance Vec3 TVec3 where
    newtype Matrix TVec3 = TMatrix (TVec3, TVec3, TVec3)
                           deriving (Eq, Show)


    fromXYZ = TVec3
    {-# INLINE fromXYZ #-}

    toXYZ (TVec3 v) = v
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = TMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (TMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}


derivingUnbox "TMatrix"
  [t|Matrix TVec3 -> (TVec3, TVec3, TVec3)|]
  [|\(TMatrix v) -> v|]
  [|TMatrix|]
