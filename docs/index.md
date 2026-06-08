# stable-haskell wasm

Install a **multi-target GHC** — native + wasm + JS in one bindist — and ship a
wasm (or JS) binary, all from a single `ghcup` install.

## Quick install

{% include-markdown "_snippets/install-channel.md" %}

You also need **Node.js 22+** on `$PATH` — both post-link and the runtime use
it. Non-TH compiles work without it; the compiler emits a clear actionable
error when TH needs it.

!!! note "Why Node 22?"
    `post-link.mjs` (the JSFFI glue generator that ships with the cross-compiler)
    uses `import.meta.filename`, which was added in **Node 20.11**. On older
    nodes (e.g. Ubuntu noble's apt `nodejs` 18.x) it silently exits without
    writing the glue, breaking JSFFI runs at instantiation time.

## What's in this channel

| Tool                              | Version                       |
|-----------------------------------|-------------------------------|
| GHC (multi-target: native + wasm + JS) | `{{ ghc_version }}`      |
| `cabal-install` (dual-compiler)   | `{{ cabal_version }}`         |

The `cabal-install` here ships a [target-prefix-aware
`guessGhcPkgFromGhcPath`](https://github.com/haskell/cabal/pull/11005)
patch so one `cabal build` invocation can drive **two GHCs** — a native one
for `Setup.hs` and Template Haskell host execution, and the wasm
cross-compiler for the package code itself.

## Host platforms

| Host                                 | Status        |
|--------------------------------------|---------------|
| `aarch64-darwin` (Apple Silicon)     | ✅ shipping  |
| `x86_64-linux`                       | ✅ shipping  |
| `aarch64-linux`                      | ✅ shipping  |
| `x86_64-darwin`, Windows             | follow-up    |

## Next steps

- :material-rocket-launch: [**Install**](install.md) — the full install path with troubleshooting links
- :material-package-down: [**hello template**](examples/hello.md) — 90 seconds to a working wasm reactor
- :material-language-javascript: [**JS + native hello**](examples/hello-js.md) — the same source on the JavaScript backend and natively
- :material-rocket: [**miso-counter template**](examples/miso-counter.md) — full interactive UI with 50+ TH-heavy deps
- :material-file-tree: [**Anatomy**](anatomy.md) — what each file in the template actually does
- :material-wrench: [**Troubleshooting**](troubleshooting.md) — common errors and fixes

## Project links

- [Release: `{{ ghc_version }}` (GHC)]({{ release_base }}/{{ ghc_version }})
- [Release: `cabal-{{ cabal_version }}`]({{ repo_url }}/releases/tag/cabal-{{ cabal_version }})
- [PR #181 — wasm cross-compile infrastructure]({{ repo_url }}/pull/181)
- [PR #184 — multi-target bindist (native + wasm + JS)]({{ repo_url }}/pull/184)
- [stable-haskell/cabal#361 — target-prefix-aware `ghc-pkg` guess](https://github.com/stable-haskell/cabal/pull/361)
- [Channel YAML]({{ channel_url }}) — the raw ghcup metadata
