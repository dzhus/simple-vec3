# Changelog

## [Unreleased]

### Changed

- `Data.Vec3.Unboxed.Contiguous` is now again `Data.Vec3.Unboxed`.
  `UVec3` was merged with `SVec3` and renamed to `CVec3`.

- `Data.Vec3.Unboxed` is now `Data.Vec3.Tupled` and renamed to
  `TVec3`.

- Benchmarks were reorganized to work well with Criterion group
  coloring

### Removed

- `SVec3` (merged into `CVec3`)

## [0.2] - 2016-10-14

### Added

- Tests

### Changed

- `Data.Vec3.Unboxed` is now `Data.Vec3.Unboxed.Contiguous`

- `Data.Vec3.TUnboxed` is now `Data.Vec3.Unboxed` and uses
  [vector-th-unbox][] to generate Unbox instance

## [0.1.0.1] - 2012-12-13

### Changed

- Benchmark is now an actual Cabal-friendly benchmark

## [0.1.0.0] - 2012-12-05

[0.2]:     https://github.com/dzhus/static-text/compare/0.1.0.1...0.2
[0.1.0.1]: https://github.com/dzhus/static-text/compare/0.1.0.0...0.1.0.1
[0.1.0.0]: https://github.com/dzhus/static-text/tree/0.1.0.0

[vector-th-unbox]: https://hackage.haskell.org/package/vector-th-unbox
