# simple-vec3

[![Travis CI build status](https://travis-ci.org/dzhus/simple-vec3.svg)](https://travis-ci.org/dzhus/simple-vec3)
[![Hackage](https://img.shields.io/hackage/v/simple-vec3.svg?colorB=5e5184&style=flat)](https://hackage.haskell.org/package/simple-vec3)
[![Hackage deps](https://img.shields.io/hackage-deps/v/simple-vec3.svg)](http://packdeps.haskellers.com/feed?needle=simple-vec3)

*Simple* three-dimensional vectors of doubles with basic vector and
matrix operations, supporting `Data.Vector.Unboxed` and
`Data.Vector.Storable`.

```haskell
>>> let v1 = CVec3 (-1) 0.0   0.2
>>> let v2 = CVec3   1  2.3   5.0
>>> let v3 = CVec3   1    1 (-0.2)
--
-- Add two vectors:
--
>>> v1 <+> v2
CVec3 0.0 2.3 5.2
--
-- Dot product:
--
>>> v1 .* v2
0.0
--
-- Multiply by a scalar:
--
>>> v1 .^ 5
CVec3 (-5.0) 0.0 1.0
--
-- Cross product:
--
>>> v1 >< v3
CVec3 (-0.2) 0.0 (-1.0)
--
-- Matrix-vector product:
--
>>> diag 2 `mxv` v2
CVec3 2.0 4.6 10.0
--
-- Interface with tuples:
--
>>> toXYZ v2
(1.0,2.3,5.0)
```

Please consult the [Hackage page for simple-vec3][hackage-doc] for full
documentation.

The package provides two different implementations for `Vec3` type
class, which differ in storage scheme. Benchmarks are included for
both. You most likely want to use `CVec3` which is based on contiguous
storage scheme and offers the best performance.

![simple-vec3 benchmarks](benchmark.png)

[hackage-doc]: https://hackage.haskell.org/package/simple-vec3/docs/Data-Vec3.html
