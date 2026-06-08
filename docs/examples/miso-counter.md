# miso-counter — full app

A real interactive [miso](https://haskell-miso.org/) app — a counter with
two buttons — built with the stable-haskell wasm cross-compiler +
dual-compiler `cabal-install`, running in your browser.

[:material-rocket-launch: Live demo (no install required)]({{ site_url }}/demos/miso-counter/){ .md-button .md-button--primary }
[:octicons-arrow-down-24: Download the template]({{ site_url }}/examples/stable-haskell-wasm-miso-counter.tar.gz){ .md-button }
[:material-source-branch: Browse files]({{ site_url }}/examples/miso-counter/){ .md-button }

This is the "step up from the [hello template](hello.md)" example: same
cabal flags, same reactor pattern, but with a full DOM-driven framework on
top and a 50+ package dependency tree pulling in `aeson`, `lens`,
`jsaddle`, `jsaddle-wasm`, `miso`, and friends — all of which exercise
Template Haskell at compile time. The Phase 6.5 verification that built
this end-to-end is what proved the dyld + `ghc-internal` `dylink.0` link
fix works at real-world scale.

## Prerequisites

If you haven't already done the [install steps](../install.md), do them first.
For convenience:

{% include-markdown "../_snippets/install-channel.md" %}

Plus:

* **Node.js 22+** — required at build time (the wasm-iserv host for Template
  Haskell evaluation runs via `node`) and for post-link
* **python3** — only for `make run-web` (serves `public/` via
  `python3 -m http.server`). Swap in any other static server you like.

## Build and run

```sh
curl -L -o miso.tar.gz {{ site_url }}/examples/stable-haskell-wasm-miso-counter.tar.gz
tar xf miso.tar.gz && cd stable-haskell-wasm-miso-counter
export PATH="$HOME/.ghcup/bin:$PATH"

make build      # FIRST run: ~5-10 min (50+ TH-heavy deps from scratch)
make run-web    # post-link + serve public/ → http://localhost:8000
make clean      # remove dist-newstyle (keeps the cabal store cache)
```

`make help` (the default target) shows all targets.

Subsequent `make build` runs after a code edit to `app/Main.hs` are
seconds-fast — only `myapp` re-builds, the dep tree is cached.

## What you should see

`http://localhost:8000` renders a counter with `+` and `-` buttons,
backed by a Haskell `Int` model in the wasm module. Clicking the buttons
sends `AddOne` / `SubOne` to a miso `Effect` that bumps the model; miso
diffs the virtual DOM and patches the real DOM.

Open the browser console — `[hs stdout]` lines are the Haskell program's
`stdout`, plumbed through `browser_wasi_shim`'s `ConsoleStdout`.

## Files that matter

* **`app/Main.hs`** — `data Action`, `type Model`, the `app :: App Model
  Action` smart-constructed via miso 1.11's `component`. The
  `foreign export javascript "hs_start" main :: IO ()` is what the JS host
  (`public/index.js`) calls after the reactor bring-up.
* **`cabal.project`** — same dual-compiler shape as the hello template,
  plus a `source-repository-package` for miso 1.11 (not always in cached
  hackage indexes) and the `allow-newer: jsaddle-wasm:ghc-experimental`
  override needed for GHC 9.14.
* **`myapp.cabal`** — wasm reactor flags (same as hello). The interesting
  part is the `if arch(wasm32) build-depends: jsaddle-wasm, ghc-experimental`
  block. `ghc-experimental` is what brings in `GHC.Wasm.Prim` (instance for
  `JSString` etc.); without it you get `GHC-87110: Could not load module
  'GHC.Wasm.Prim'`.
* **`public/index.js`** — same reactor bring-up as the hello template,
  using `browser_wasi_shim` instead of `node:wasi`.

## Compared to the hello template

|                            | hello                  | miso-counter                                |
|----------------------------|------------------------|---------------------------------------------|
| TH at compile time         | none                   | aeson, lens, jsaddle, miso, …               |
| First `make build`         | seconds                | ~5–10 minutes                                |
| Where it runs              | node:wasi OR browser   | browser only (needs DOM)                    |
| wasm size                  | ~1.7 MB                | ~2.8 MB                                      |
| What it proves             | reactor + JSFFI work   | the full TH/dyld/link chain works at scale  |

## License

Apache-2.0.
