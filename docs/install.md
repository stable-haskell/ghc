# Install

## Prerequisites

* **[ghcup](https://www.haskell.org/ghcup/)** — the Haskell toolchain
  installer. Install it via the official one-liner if you haven't already:

    ```sh
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    ```

* **Node.js 22+** — required at build time (the wasm-iserv host for Template
  Haskell evaluation runs via `node`) and for `post-link.mjs`.

    !!! warning "Ubuntu's apt nodejs is too old"
        Ubuntu noble's apt `nodejs` package is 18.19.1 — `post-link.mjs`
        uses `import.meta.filename` (added in Node 20.11) and will
        silently exit without writing the JSFFI glue. On Ubuntu, install
        Node 22 from NodeSource:

        ```sh
        curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash -
        sudo apt-get install -y --no-install-recommends nodejs
        ```

* **python3** — for the `make run-web` static server in the templates (any
  static server works; this just uses `python3 -m http.server`).

* **wasi-sdk** *(for the wasm target)* — the wasm GHC's C compiler (`wasm32-unknown-wasi-clang`)
  lives here. We deliberately don't bundle wasi-sdk in the ghcup channel —
  its version pinning is [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta)'s
  domain, and we'd rather defer to a single source of truth. Install via
  the upstream bootstrap script:

    ```sh
    curl -fsSL https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/raw/master/bootstrap.sh \
      | FLAVOUR=9.12 PREFIX=$HOME/.ghc-wasm sh
    export PATH="$HOME/.ghc-wasm/wasi-sdk/bin:$PATH"
    ```

    !!! note "wasi-sdk prefix bridge"
        wasi-sdk's binaries are named `wasm32-wasi-clang` etc., but the
        wasm GHC was configured with the canonical autoconf triple
        `wasm32-unknown-wasi`. After installing wasi-sdk, create the
        missing prefix-bridge symlinks:

        ```sh
        WASI_BIN="$HOME/.ghc-wasm/wasi-sdk/bin"
        for tool in clang clang++; do
          ln -sf "$WASI_BIN/wasm32-wasi-$tool" "$WASI_BIN/wasm32-unknown-wasi-$tool"
        done
        for tool in ar nm ranlib strip; do
          ln -sf "$WASI_BIN/llvm-$tool" "$WASI_BIN/wasm32-unknown-wasi-$tool"
        done
        ```

        Once we ship a `ghc98-minimal-ghc-web` devx flavor (see
        [stable-haskell/devx#250](https://github.com/input-output-hk/devx/pull/250)),
        this dance disappears.

* **emscripten** *(for the JS target)* — the JS backend's C toolchain, the
  JS analogue of wasi-sdk. Install + activate:

    ```sh
    git clone --depth 1 --branch 3.1.74 https://github.com/emscripten-core/emsdk.git
    cd emsdk && ./emsdk install 3.1.74 && ./emsdk activate 3.1.74 && source ./emsdk_env.sh
    ```

## Install the toolchain

{% include-markdown "_snippets/install-channel.md" %}

`ghcup set` (above) symlinks every per-target binary — `ghc` (native),
`wasm32-unknown-wasi-*`, and `javascript-unknown-ghcjs-*` — into `~/.ghcup/bin`,
which ghcup keeps on your `$PATH`. If it isn't already there, add it:

```sh
export PATH="$HOME/.ghcup/bin:$PATH"
```

## Verify

```sh
$ ghc --version                          # native
The Glorious Glasgow Haskell Compilation System, version 9.14.0
$ wasm32-unknown-wasi-ghc --version      # wasm cross
The Glorious Glasgow Haskell Compilation System, version 9.14.0
$ javascript-unknown-ghcjs-ghc --version # JS cross
The Glorious Glasgow Haskell Compilation System, version 9.14.0
$ cabal --version
cabal-install version {{ cabal_version }}
```

If both commands print versions, you're ready for the [hello
template](examples/hello.md).

## Host platforms

| Host                                | Status      | Tarball |
|-------------------------------------|-------------|---------|
| `aarch64-darwin` (Apple Silicon)    | ✅ shipping | [`ghc-multi-target-aarch64-darwin.tar.gz`]({{ release_base }}/{{ ghc_version }}/ghc-multi-target-aarch64-darwin.tar.gz) |
| `x86_64-linux`                      | ✅ shipping | [`ghc-multi-target-x86_64-linux.tar.gz`]({{ release_base }}/{{ ghc_version }}/ghc-multi-target-x86_64-linux.tar.gz) |
| `aarch64-linux`                     | ✅ shipping | [`ghc-multi-target-aarch64-linux.tar.gz`]({{ release_base }}/{{ ghc_version }}/ghc-multi-target-aarch64-linux.tar.gz) |
| `x86_64-darwin`, Windows            | follow-up   | — |

The package-db binary cache (`package.cache`) is regenerated automatically by
`ghc-pkg` on first read (mtime-based), so no relocate step is required after
install — `ghcup` does the right thing out of the box.

## What's installed

```text
$HOME/.ghcup/
├── bin/                        # ghcup symlinks: ghc, wasm32-unknown-wasi-*,
│                               #   javascript-unknown-ghcjs-*, cabal, …
├── ghc/{{ ghc_version }}/
│   ├── bin/                    # one binary, three targets (argv[0] dispatch)
│   ├── lib/                    # native package db + libraries
│   ├── lib/targets/
│   │   ├── wasm32-unknown-wasi/lib/        # wasm libs (+ post-link.mjs, dyld.mjs)
│   │   └── javascript-unknown-ghcjs/lib/   # JS libs
│   └── share/                  # settings, GHCi templates, etc.
└── cabal/{{ cabal_version }}/
    └── bin/cabal               # dual-compiler aware cabal-install
```

Next: [build the hello template](examples/hello.md).
