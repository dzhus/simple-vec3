{-# LANGUAGE TypeFamilies #-}

module Data.Vec3.Class
    ( Vec3(..)
    )

where

class Vec3 v where
    origin    :: v
    fromXYZ   :: Double -> Double -> Double -> v

    (<+>)     :: v -> v -> v
    (<->)     :: v -> v -> v
    (><)      :: v -> v -> v
    (.^)      :: v -> Double -> v
    (.*)      :: v -> v -> Double

    norm      :: v -> Double
    norm v = sqrt (v .* v)
    {-# INLINE norm #-}

    normalize :: v -> v
    normalize v = v .^ (1 / norm v)
    {-# INLINE normalize #-}

    distance  :: v -> v -> Double
    distance v1 v2 = norm (v1 <-> v2)
    {-# INLINE distance #-}

    invert    :: v -> v
    invert v  =  v .^ (-1)
    {-# INLINE invert #-}

