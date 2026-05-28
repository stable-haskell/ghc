# hello — 90 seconds

A minimal Haskell-to-WebAssembly "hello world" you can run under Node.js *or*
in a browser, built with the stable-haskell wasm cross-compiler + dual-compiler
`cabal-install`.

[:octicons-arrow-down-24: Download the template]({{ site_url }}/examples/stable-haskell-wasm-hello.tar.gz){ .md-button }
[:material-source-branch: Browse files]({{ site_url }}/examples/hello/){ .md-button }

## Prerequisites

If you haven't already done the [install steps](../install.md), do them first.
For convenience:

{% include-markdown "../_snippets/install-channel.md" %}

Plus **Node.js 22+** on `$PATH`.

## Build and run

```sh
curl -L -o hello.tar.gz {{ site_url }}/examples/stable-haskell-wasm-hello.tar.gz
tar xf hello.tar.gz && cd stable-haskell-wasm-hello
export PATH="$HOME/.ghcup/ghc/{{ wasm_version }}/bin:$PATH"

make build      # cabal build via the wasm cross-compiler
make run-node   # post-link + node run.mjs       → prints "Hello from the WASM reactor!"
make run-web    # post-link + serve public/      → http://localhost:8000
make clean      # remove build artifacts
```

`make help` (the default target) shows all targets.

## What's interesting in here

* **`myapp.cabal`** — the WASM-only `ghc-options` block sets up the WASI
  **reactor pattern**: `-no-hs-main` + `-optl-mexec-model=reactor` +
  `-optl-Wl,--export=hs_start`. The host JS calls `hs_start()` instead of
  the wasm running its own `_start`.
* **`cabal.project`** — declares the **dual-compiler** form:
  `with-build-compiler: ghc` (native) for `Setup.hs` and TH host;
  `with-compiler: wasm32-unknown-wasi-ghc` for the actual package code.
* **`app/Main.hs`** — `foreign export javascript "hs_start" main :: IO ()` is
  the surface that the JS host invokes.
* **`run.mjs` / `public/index.js`** — the **reactor bring-up sequence**:
  `wasi.initialize` → `__ghc_wasm_jsffi_init` → `hs_start`. All three steps
  are mandatory; skipping `__ghc_wasm_jsffi_init` yields *"RTS is not
  initialised; call hs_init() first"*.
* **`Makefile`** — wires `post-link.mjs` (ships in
  `~/.ghcup/ghc/{{ wasm_version }}/lib/targets/wasm32-unknown-wasi/lib/`) into
  the build so you don't have to remember the path. Override `WASM_VERSION`
  if you need a different one.

## Want more?

Step up to the [miso-counter template](miso-counter.md) — same recipe, but
with 50+ Template-Haskell-heavy deps (`aeson`, `lens`, `jsaddle`, `miso`)
exercising the full dyld + JSFFI link chain at real-world scale.

## License

Apache-2.0.
