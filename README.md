# stable-haskell wasm — gh-pages source

This branch holds the **markdown source** for the
[stable-haskell.github.io/ghc/](https://stable-haskell.github.io/ghc/) site,
plus the workflow that builds it. The generated HTML lives on the
`gh-pages` branch (auto-managed; don't hand-edit).

## Structure

```
mkdocs.yml                 site config (theme, plugins, nav)
docs/
  index.md                 home page
  install.md               install instructions
  examples/
    hello.md               90-second template walkthrough
    miso-counter.md        full-app template walkthrough
  anatomy.md               what each file in a template does
  troubleshooting.md       common errors and fixes
  _macros.py               version vars exposed as {{ wasm_version }} etc.
  _snippets/
    install-channel.md     shared install incantation
.github/workflows/docs.yml workflow: build → deploy to gh-pages
```

## Editing

```sh
# Local preview with live reload
pip install mkdocs-material mkdocs-macros-plugin mkdocs-include-markdown-plugin pymdown-extensions
mkdocs serve
# → http://localhost:8000

# Production build (same as CI)
mkdocs build --strict
```

## Deploying

Push to `gh-pages-source`. The
[`Deploy docs`](.github/workflows/docs.yml) workflow fires, builds the
site, *additively* carries over the static artifacts (channel YAML, live
demo, downloadable tarballs, raw template source files) from the current
`gh-pages` HEAD, and deploys to `gh-pages`.

## Bumping versions

When a new GHC or cabal lands on the channel:

1. Edit `docs/_macros.py` — bump `wasm_version` / `cabal_version`.
2. Push. Every page that uses `{{ wasm_version }}` picks it up.

That's the DRY win over the previous hand-written HTML: a release bump is
a one-line edit, not a 12-occurrences-across-5-files chore.

## What's NOT on this branch

* `ghcup-wasm.yaml` — that lives on `gh-pages` directly (it's a release
  artifact, not docs source).
* `demos/miso-counter/` — the live demo with `myapp.wasm` is a build
  artifact, hand-deployed once per release. Lives on `gh-pages`.
* `examples/stable-haskell-wasm-*.tar.gz` — downloadable template
  tarballs. Lives on `gh-pages`.
* `examples/{hello,miso-counter}/{app,public,Makefile,cabal.project,myapp.cabal,run.mjs}`
  — the raw template source files (linked from anatomy/troubleshooting
  docs so users can browse). Live on `gh-pages`.

The deploy workflow preserves all of the above when it builds the docs.
