# hello (JS + native) — the same source, two more targets

The multi-target GHC builds the *same* Haskell source for the browser's
**JavaScript** backend and for your **native** host — no extra install beyond
the [multi-target toolchain](../install.md). (The wasm target uses a different,
reactor-shaped template — see [hello (wasm)](hello.md).)

[:octicons-arrow-down-24: Download the template]({{ site_url }}/examples/stable-haskell-js-hello.tar.gz){ .md-button }

## Prerequisites

The [install steps](../install.md) (multi-target GHC + cabal), plus:

* **Node.js 22+** — runs the JS output.
* **emscripten** — the JS backend's C toolchain (the JS analogue of wasi-sdk).
  Install + activate:

    ```sh
    git clone --depth 1 --branch 3.1.74 https://github.com/emscripten-core/emsdk.git
    cd emsdk && ./emsdk install 3.1.74 && ./emsdk activate 3.1.74 && source ./emsdk_env.sh
    ```

## Build and run

```sh
curl -L -o js-hello.tar.gz {{ site_url }}/examples/stable-haskell-js-hello.tar.gz
tar xf js-hello.tar.gz && cd stable-haskell-js-hello

make run-node     # cabal build for JS  → node  → prints the greeting
make run-native   # same source via ghc → host  → prints the greeting
make clean
```

`make help` (the default target) shows all targets.

## What's interesting in here

* **`cabal.project`** pins `with-compiler: javascript-unknown-ghcjs-ghc` — the
  JS frontend of the multi-target binary (`argv[0]` dispatch). One install,
  three compilers; this selects the JS one.
* **`app/Main.hs`** is a plain `main = putStrLn …` — no FFI, no reactor. The JS
  backend emits a node-runnable program, which `make run-node` runs via
  `node "$(cabal list-bin exe:myapp)"`.
* **`make run-native`** builds the identical source with the native `ghc` from
  the same install — the third target, no project changes.

## What about a JS miso app?

Template-Haskell-heavy packages (miso, lens, aeson, …) don't build for the JS
target yet on this bindist: the JS external interpreter that evaluates TH
splices is missing some posix constants (`SIZEOF_STRUCT_STAT is not defined`).
For a full interactive UI today, use the wasm
[miso-counter](miso-counter.md) template; a JS miso example will land once
TH-on-JS is fixed.

## License

Apache-2.0.
