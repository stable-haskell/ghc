-- MicroHs stage1 shim: empty module satisfying ordering-only `import GHC.Base ()`
-- in .hs-boot / compiler files (see W1 of Note [Tracking dependencies on primitives]).
module GHC.Base () where
