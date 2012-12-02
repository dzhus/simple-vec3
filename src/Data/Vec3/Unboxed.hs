{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

Suitable for use with "Data.Vector.Unboxed".

-}

module Data.Vec3.Unboxed
    ( UVec3(..)
    -- * Matrix operations
    , Matrix(..)
    , mxv
    , vxv
    , dotM
    , diag
    , addM
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
    origin = UVec3 (0, 0, 0)
    {-# INLINE origin #-}

    fromXYZ x y z = UVec3 (x, y, z)
    {-# INLINE fromXYZ #-}

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


-- | Matrix given by its rows.
newtype Matrix = Matrix (UVec3, UVec3, UVec3)
    deriving (Eq, Show,
              VG.Vector VU.Vector,
              VG.MVector VU.MVector,
              VU.Unbox)


-- | Generic vector dot product.
--
-- Multiply transpose of first vector by given matrix, then multiply
-- the result by second vector.
dotM :: UVec3 -> UVec3 -> Matrix -> Double
dotM v1 v2 m = v1 .* (m `mxv` v2)


-- | Multiply matrix (given by row vectors) and vector
mxv :: Matrix -> UVec3 -> UVec3
mxv (Matrix (r1, r2, r3)) v = UVec3 (r1 .* v, r2 .* v, r3 .* v)


-- | Produce matrix with diagonal elements equal to given value.
diag :: Double -> Matrix
diag d = Matrix (UVec3 (d, 0, 0),
                 UVec3 (0, d, 0),
                 UVec3 (0, 0, d))


-- | Transpose vector and multiply it by another vector, producing a
-- matrix.
vxv :: UVec3 -> UVec3 -> Matrix
vxv (UVec3 (v11, v12, v13)) v2 = Matrix (v2 .^ v11, v2 .^ v12, v2 .^ v13)


-- | Add two matrices.
addM :: Matrix -> Matrix -> Matrix
addM (Matrix (r11, r12, r13)) (Matrix (r21, r22, r23)) =
    -- We could add Applicative instance for Matrix and lift (+) to it.
    Matrix (r11 <+> r21, r12 <+> r22, r13 <+> r23)
