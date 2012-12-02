{-# LANGUAGE TypeFamilies #-}

module Data.Vec3.Class
    ( Vec3(..)
    )

where

-- | Three-dimensional vector and matrix.
class Vec3 v where
    data Matrix v

    -- | Origin point @(0, 0, 0)@.
    origin         :: v

    -- | Construct a new vector from components.
    fromXYZ        :: (Double, Double, Double) -> v

    -- | Deconstruct a vector into components.
    toXYZ          :: v -> (Double, Double, Double)

    -- | Add two vectors.
    (<+>)          :: v -> v -> v

    -- | Subtract two vectors.
    (<->)          :: v -> v -> v

    -- | Cross product.
    (><)           :: v -> v -> v

    -- | Scale a vector.
    (.^)           :: v -> Double -> v

    -- | Dot product.
    (.*)           :: v -> v -> Double

    -- | Euclidean norm of a vector.
    norm           :: v -> Double
    norm v          = sqrt (v .* v)
    {-# INLINE norm #-}

    -- | Produce unit vector with the same direction as the original
    -- one.
    normalize      :: v -> v
    normalize v     = v .^ (1 / norm v)
    {-# INLINE normalize #-}

    -- | Distance between two points.
    distance       :: v -> v -> Double
    distance v1 v2  = norm (v1 <-> v2)
    {-# INLINE distance #-}

    -- | Scale vector by -1.
    invert         :: v -> v
    invert v        = v .^ (-1)
    {-# INLINE invert #-}


    -- | Construct a new matrix from rows.
    fromRows       :: (v, v, v) -> Matrix v

    -- | Deconstruct a matrix into rows.
    toRows         :: Matrix v -> (v, v, v)

    -- | Generic vector dot product.
    --
    -- Multiply the transpose of the first vector by the given matrix,
    -- then multiply the result by the second vector.
    dotM           :: v -> v -> Matrix v -> Double
    dotM v1 v2 m    = v1 .* (m `mxv` v2)
    {-# INLINE dotM #-}

    -- | Multiply matrix and vector.
    mxv            :: Matrix v -> v -> v
    mxv m v         = case toRows m of
                        (r1, r2, r3) -> fromXYZ (r1 .* v, r2 .* v, r3 .* v)
    {-# INLINE mxv #-}

    -- | Build a diagonal matrix from a number.
    diag           :: Double -> Matrix v
    diag d          = fromRows
                      (fromXYZ (d, 0, 0),
                       fromXYZ (0, d, 0),
                       fromXYZ (0, 0, d))
    {-# INLINE diag #-}

    -- | Transpose vector and multiply it by another vector, producing a
    -- matrix.
    vxv            :: v -> v -> Matrix v
    vxv v1 v2       = case toXYZ v1 of
                        (v11, v12, v13) ->
                            fromRows (v2 .^ v11, v2 .^ v12, v2 .^ v13)
    {-# INLINE vxv #-}

    -- | Add two matrices.
    addM           :: Matrix v -> Matrix v -> Matrix v
    addM m1 m2      = case (toRows m1, toRows m2) of
                        ((r11, r12, r13),
                         (r21, r22, r23)) -> fromRows (r11 <+> r21,
                                                       r12 <+> r22,
                                                       r13 <+> r23)
    {-# INLINE addM #-}
