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

* **wasi-sdk** — the wasm GHC's C compiler (`wasm32-unknown-wasi-clang`)
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
        ```

        Once we ship a `ghc98-minimal-ghc-web` devx flavor (see
        [stable-haskell/devx#250](https://github.com/input-output-hk/devx/pull/250)),
        this dance disappears.

## Install the toolchain

{% include-markdown "_snippets/install-channel.md" %}

Then add the cross-compiler bin dir to your `$PATH` so the
`wasm32-unknown-wasi-*` binaries resolve:

```sh
export PATH="$HOME/.ghcup/ghc/{{ wasm_version }}/bin:$PATH"
```

## Verify

```sh
$ wasm32-unknown-wasi-ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.14.0
$ cabal --version
cabal-install version {{ cabal_version }}
```

If both commands print versions, you're ready for the [hello
template](examples/hello.md).

## Host platforms

| Host                                | Status      | Tarball |
|-------------------------------------|-------------|---------|
| `aarch64-darwin` (Apple Silicon)    | ✅ shipping | [`ghc-wasm32-unknown-wasi-aarch64-darwin.tar.gz`]({{ release_base }}/{{ wasm_version }}/ghc-wasm32-unknown-wasi-aarch64-darwin.tar.gz) |
| `x86_64-linux`                      | ✅ shipping | [`ghc-wasm32-unknown-wasi-x86_64-linux.tar.gz`]({{ release_base }}/{{ wasm_version }}/ghc-wasm32-unknown-wasi-x86_64-linux.tar.gz) |
| `aarch64-linux`                     | ✅ shipping | [`ghc-wasm32-unknown-wasi-aarch64-linux.tar.gz`]({{ release_base }}/{{ wasm_version }}/ghc-wasm32-unknown-wasi-aarch64-linux.tar.gz) |
| `x86_64-darwin`, Windows            | follow-up   | — |

The package-db binary cache (`package.cache`) is regenerated automatically by
`ghc-pkg` on first read (mtime-based), so no relocate step is required after
install — `ghcup` does the right thing out of the box.

## What's installed

```text
$HOME/.ghcup/
├── ghc/{{ wasm_version }}/
│   ├── bin/                    # wasm32-unknown-wasi-* binaries
│   ├── lib/targets/
│   │   └── wasm32-unknown-wasi/
│   │       ├── lib/            # the actual cross-compiler libraries
│   │       ├── lib/dyld.mjs    # JS dyld shim for Template Haskell
│   │       └── lib/post-link.mjs   # JSFFI glue generator
│   └── share/                  # GHCi templates, settings, etc.
└── cabal/{{ cabal_version }}/
    └── bin/cabal               # dual-compiler aware cabal-install
```

Next: [build the hello template](examples/hello.md).
