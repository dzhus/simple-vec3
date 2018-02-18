{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Vec3


infix 4 <~=>
(<~=>) :: CVec3 -> CVec3 -> Bool
(<~=>) a b = ax ~= bx &&
             ay ~= by &&
             az ~= bz
  where
    (ax, ay, az) = toXYZ (a :: CVec3)
    (bx, by, bz) = toXYZ (b :: CVec3)


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
    (\(a :: CVec3) b -> a <+> b <~=> b <+> a)
  , testProperty
    "Associativity of addition: (a + b) + c = a + (b + c)"
    (\(a :: CVec3) b c -> (a <+> b) <+> c <~=> a <+> (b <+> c))
  , testProperty
    "Identity element of addition (zero): v + 0 = v"
    (\(v :: CVec3) -> (v <+> origin <~=> v))
  , testProperty
    "Inverse elements of addition: v + (-v) = 0"
    (\(v :: CVec3) -> (v <+> invert v <~=> origin))
  , testProperty
    "Compatibility of scalar and field multiplication"
    (\(v :: CVec3) p q -> (v .^ p .^ q <~=> v .^ (p * q)))
  , testProperty
    "Identity of scalar multiplication"
    (\(v :: CVec3) -> (v .^ 1 <~=> v))
  , testProperty
    "Distributivity wrt vector addition"
    (\(a :: CVec3) b p -> ((a <+> b) .^ p) <~=> (a .^ p) <+> (b .^ p))
  , testProperty
    "Distributivity wrt scalar addition"
    (\(a :: CVec3) p q -> (a .^ (p + q) <~=> (a .^ p) <+> (a .^ q)))
  , testProperty
    "Subtraction definition"
    (\(a :: CVec3) b -> (a <+> invert b <~=> a <-> b))
  , testProperty
    "Normalization"
    (\(v :: CVec3) -> (v <~=> origin || norm (normalize v) ~= 1))
  , testProperty
    "Triangle inequality"
    (\(a :: CVec3) b c -> (distance a b + distance b c >= distance a c))
  ]


main :: IO ()
main = defaultMain $ testGroup "Tests" tests
