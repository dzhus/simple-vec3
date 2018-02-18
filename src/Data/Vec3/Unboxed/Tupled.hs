{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vec3.Unboxed.Tupled
    ( TUVec3(..)
    )

where

import Prelude

import Data.Vector.Unboxed.Deriving

import Data.Vec3.Class


-- | 'Vec3' implementation with "Data.Vector.Unboxed.Unbox" instance
-- based on default Unbox instance for tuples of arrays, which wraps a
-- vector of tuples as a tuple of vectors.
--
-- @
-- interface:  [d1 (x, y, z); d2 (x, y, z) ...], length = N
--                  |  |  |       |  |  |
-- storage(x): [d1x-+  |  | ; d2x-+  |  |  ...], length = N
-- storage(y): [d1y----+  | ; d2y----+  |  ...], length = N
-- storage(z): [d1z-------+ ; d2z-------+  ...], length = N
-- @
newtype TUVec3 = TUVec3 (Double, Double, Double)
               deriving (Eq, Show)


derivingUnbox "TUVec3"
  [t|TUVec3 -> (Double, Double, Double)|]
  [|\(TUVec3 v) -> v|]
  [|TUVec3|]


instance Vec3 TUVec3 where
    newtype Matrix TUVec3 = UMatrix (TUVec3, TUVec3, TUVec3)
                           deriving (Eq, Show)


    fromXYZ = TUVec3
    {-# INLINE fromXYZ #-}

    toXYZ (TUVec3 v) = v
    {-# INLINE toXYZ #-}

    fromRows (r1, r2, r3) = UMatrix (r1, r2, r3)
    {-# INLINE fromRows #-}

    toRows (UMatrix (r1, r2, r3)) = (r1, r2, r3)
    {-# INLINE toRows #-}


derivingUnbox "UMatrix"
  [t|Matrix TUVec3 -> (TUVec3, TUVec3, TUVec3)|]
  [|\(UMatrix v) -> v|]
  [|UMatrix|]
