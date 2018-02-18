# simple-vec3

[![Travis CI build status](https://travis-ci.org/dzhus/simple-vec3.svg)](https://travis-ci.org/dzhus/simple-vec3)
[![Hackage](https://img.shields.io/hackage/v/simple-vec3.svg?colorB=5e5184&style=flat)](https://hackage.haskell.org/package/simple-vec3)
[![Hackage deps](https://img.shields.io/hackage-deps/v/simple-vec3.svg)](http://packdeps.haskellers.com/feed?needle=simple-vec3)

*Simple* three-dimensional vectors of doubles with basic vector and
matrix operations, supporting `Data.Vector.Unboxed` and
`Data.Vector.Storable`.

Please consult the [Hackage page for simple-vec3][hackage-doc] for full
documentation.

The package provides two different implementations for `Vec3` type
class, which differ in storage scheme. Benchmarks are included for
both. You most likely want to use `CVec3` which is based on contiguous
storage scheme and offers the best performance.

![simple-vec3 benchmarks](benchmark.png)

[hackage-doc]: https://hackage.haskell.org/package/simple-vec3/docs/Data-Vec3.html
