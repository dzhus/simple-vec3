{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Vec3


infix 4 <~=>
(<~=>) :: SVec3 -> SVec3 -> Bool
(<~=>) a b = ax ~= bx &&
             ay ~= by &&
             az ~= bz
  where
    (ax, ay, az) = toXYZ (a :: SVec3)
    (bx, by, bz) = toXYZ (b :: SVec3)


infix 4 ~=
(~=) :: Double -> Double -> Bool
(~=) a b | a == b = True
         | a == 0 || b == 0 || isDenormalized absDiff = absDiff < maxError
         | otherwise = absDiff / (abs a + abs b) < maxError
  where
    absDiff = abs $ a - b
    maxError = 1e-13


tests :: [TestTree]
tests =
  [ testProperty
    "Commutativity of addition: a + b = b + a"
    (\(a :: SVec3) b -> a <+> b <~=> b <+> a)
  , testProperty
    "Associativity of addition: (a + b) + c = a + (b + c)"
    (\(a :: SVec3) b c -> (a <+> b) <+> c <~=> a <+> (b <+> c))
  , testProperty
    "Identity element of addition (zero): v + 0 = v"
    (\(v :: SVec3) -> (v <+> origin <~=> v))
  , testProperty
    "Inverse elements of addition: v + (-v) = 0"
    (\(v :: SVec3) -> (v <+> invert v <~=> origin))
  , testProperty
    "Compatibility of scalar and field multiplication"
    (\(v :: SVec3) p q -> (v .^ p .^ q <~=> v .^ (p * q)))
  , testProperty
    "Identity of scalar multiplication"
    (\(v :: SVec3) -> (v .^ 1 <~=> v))
  , testProperty
    "Distributivity wrt vector addition"
    (\(a :: SVec3) b p -> ((a <+> b) .^ p <~=> a .^ p <+> b .^ p))
  , testProperty
    "Distributivity wrt scalar addition"
    (\(a :: SVec3) p q -> (a .^ (p + q) <~=> a .^ p <+> a .^ q))
  , testProperty
    "Subtraction definition"
    (\(a :: SVec3) b -> (a <+> invert b <~=> a <-> b))
  , testProperty
    "Normalization"
    (\(v :: SVec3) -> (v <~=> origin || norm (normalize v) ~= 1))
  , testProperty
    "Triangle inequality"
    (\(a :: SVec3) b c -> (distance a b + distance b c >= distance a c))
  ]


main :: IO ()
main = defaultMain $ testGroup "Tests" tests
