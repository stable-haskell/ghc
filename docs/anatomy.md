# Anatomy of the template

Every wasm-cross template (whether [hello](examples/hello.md) or
[miso-counter](examples/miso-counter.md)) is built from the same four moving
parts. Once you understand them, scaling to your own app is just adding deps.

## `cabal.project` — dual-compiler form

One `cabal build` drives **two GHCs**: a native one for `Setup.hs` + TH host,
and the wasm cross-compiler for the package code.

```cabal
with-build-compiler: ghc
with-build-hc-pkg:   ghc-pkg
with-compiler:       wasm32-unknown-wasi-ghc
with-hc-pkg:         wasm32-unknown-wasi-ghc-pkg

if arch(wasm32)
  shared: True
```

This is what the [stable-haskell/cabal#361 target-prefix-aware
`guessGhcPkgFromGhcPath`](https://github.com/stable-haskell/cabal/pull/361)
patch unlocks. Older `cabal-install` versions guess the wrong `ghc-pkg`
binary and bail with a version-mismatch error.

## `myapp.cabal` — the WASI reactor incantation

Don't auto-export Haskell's `main`, build a long-lived reactor module, and let
the linker export our `hs_start` FFI symbol so the JS host can call it.

```cabal
if arch(wasm32)
  ghc-options:
    -no-hs-main
    -optl-mexec-model=reactor
    "-optl-Wl,--export=hs_start"
  cpp-options: -DWASM
```

* `-no-hs-main` — don't generate a default `main` entry; we provide our own
  via `foreign export javascript`.
* `-optl-mexec-model=reactor` — link as a WASI **reactor** (long-lived,
  callable from JS) rather than a one-shot **command** module.
* `-optl-Wl,--export=hs_start` — instruct `wasm-ld` to keep our entry
  symbol in the wasm exports table.

For TH-heavy apps you'll also want
`build-depends: ghc-experimental` (brings `GHC.Wasm.Prim`) and
`jsaddle-wasm` if you're using anything jsaddle-flavoured. See the
[miso-counter template](examples/miso-counter.md) for the full incantation.

## `app/Main.hs` — the FFI entry

```haskell
{-# LANGUAGE CPP #-}
module Main where

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = putStrLn "Hello from the WASM reactor!"
```

The `#ifdef WASM` guard means the same source builds natively too —
useful for unit-testing the same `Main` outside the wasm pipeline.

## `run.mjs` / `public/index.js` — the reactor bring-up sequence

The mandatory three-step dance:

```js
wasi.initialize(instance);              // 1. WASI snapshot prep
instance.exports.__ghc_wasm_jsffi_init();   // 2. install jsffi imports
await instance.exports.hs_start();      // 3. run Haskell main
```

Skipping step 2 (the most common mistake) yields:

> RTS is not initialised; call hs_init() first

— which is misleading because the actual missing call is
`__ghc_wasm_jsffi_init`, not `hs_init`. See the
[troubleshooting page](troubleshooting.md) for the full failure mode.

## How it scales

The reactor pattern scales **unchanged** from `hello` to `miso-counter` to
any miso app you want to build — the compiler, linker, JSFFI, and runtime
pieces are identical. What changes is just `build-depends` and the
`Main.hs` body.

The [miso-counter template](examples/miso-counter.md) pins miso 1.11 via
`source-repository-package` and shows the `allow-newer:
jsaddle-wasm:ghc-experimental` override needed for GHC 9.14 — adapt that
recipe to other miso apps or other TH-heavy libraries.
