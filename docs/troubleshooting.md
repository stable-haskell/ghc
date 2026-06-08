# Troubleshooting

Common errors and fixes, ordered roughly by how often we've seen them.

## Install / setup

### `wasm32-unknown-wasi-ghc: command not found`

The multi-target GHC is installed but its symlinks aren't on `PATH`. Run
`ghcup set ghc {{ ghc_version }}` to (re)create the per-target symlinks in
`~/.ghcup/bin`, then make sure that dir is on `PATH`:

```sh
export PATH="$HOME/.ghcup/bin:$PATH"
```

For permanent use, add that line to your `~/.bashrc` / `~/.zshrc` etc.

### `Version mismatch between ghc and ghc-pkg`

Using an upstream `cabal` (or any version older than `{{ cabal_version }}`)
without the [target-prefix-aware
`guessGhcPkgFromGhcPath`](https://github.com/stable-haskell/cabal/pull/361)
patch — it picks `ghc-pkg` (native) when it should pick
`wasm32-unknown-wasi-ghc-pkg`.

```sh
ghcup install cabal {{ cabal_version }}
ghcup set    cabal {{ cabal_version }}
```

## Build-time

### `Template Haskell evaluation on the wasm32 backend requires` ***`node`***

Self-explanatory — install Node.js 22+ and ensure it's on `$PATH`. On
Ubuntu noble, the apt-shipped `nodejs` is 18.x and won't work
([see below](#post-linkmjs-runs-but-produces-nothing)); install from
NodeSource instead:

```sh
curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash -
sudo apt-get install -y --no-install-recommends nodejs
```

### `External interpreter terminated (127)`

The wasm-iserv host (which Template Haskell shells out to) failed to
start. Almost always `node` missing from `$PATH`. The compiler emits a
clearer message about this in `9.14.0.stable.0` and later, but if you see
the bare `(127)` you're probably on something older.

### `Could not load module 'GHC.Wasm.Prim'`

Missing `ghc-experimental` build-dep — `GHC.Wasm.Prim` (which provides the
`JSString` and friend instances) lives there. Add to your wasm
`build-depends`:

```cabal
if arch(wasm32)
  build-depends: ghc-experimental, jsaddle-wasm
```

The [miso-counter template](examples/miso-counter.md) has this already.

### miso compile errors like `Not in scope: 'initialAction'`

You're using a pre-1.11 miso API in your `Main.hs`. miso 1.11 removed
`initialAction` / `events` from `App`; you now pass `defaultEvents` as
the first arg to `startApp`, and smart-construct via `component`. See
the [miso-counter template's `app/Main.hs`]({{ site_url }}/examples/miso-counter/app/Main.hs)
for the working pattern.

## Post-link / runtime

### `post-link.mjs` runs but produces nothing

You're on Node 18 (or any pre-20.11). `post-link.mjs` uses
`import.meta.filename`, which was added in Node 20.11; on older nodes its
`isMain()` check returns false silently and the script exits without
writing the `.mjs` glue.

Symptom downstream: when you then try to run the wasm via
`node:wasi`, you get `Cannot find module '/tmp/hello.mjs'` or similar.

Fix: install Node 22 from NodeSource (snippet above).

### `RTS is not initialised; call hs_init() first`

The host JS skipped `instance.exports.__ghc_wasm_jsffi_init()` between
`wasi.initialize` and `hs_start`. The error message names `hs_init` but
the missing call is `__ghc_wasm_jsffi_init` (a JSFFI-specific imports
installer).

Fix: the [hello template's `run.mjs`](examples/hello.md) has the canonical
three-step bring-up sequence:

```js
wasi.initialize(instance);                  // 1
instance.exports.__ghc_wasm_jsffi_init();   // 2 — DON'T skip this
await instance.exports.hs_start();          // 3
```

### Browser console: 404 on `myapp.wasm` or `jsffi.mjs`

Both files must live next to `index.html` for the relative `fetch()` in
`public/index.js` to find them. `make run-web` does this layout for you;
if you're running your own static server, ensure all three are in the
same directory.

## Still stuck?

* [File an issue]({{ repo_url }}/issues) — include `ghc --version`,
  `cabal --version`, `node --version`, your platform, and the full
  error message.
* The wasm-cross-ghcup
  [running status log]({{ repo_url }}/blob/feat/wasm-cross-ghcup/lode/wasm-cross-ghcup-plan.md)
  documents most failure modes we've hit and fixed.
