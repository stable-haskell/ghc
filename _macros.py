"""
mkdocs-macros variables — single source of truth for version strings, URLs,
and the names that change between releases.

Bump these here when a new GHC or cabal lands on the channel — every page
that uses {{ ghc_version }} / {{ cabal_version }} updates automatically.
"""


def define_env(env):
    # Versions exposed in install commands / PATH exports / changelog links.
    # ghc_version is the MULTI-TARGET bindist (native + wasm + JS in one).
    env.variables["ghc_version"]   = "multi-9.14.0.stable.2"
    env.variables["cabal_version"] = "3.17.0.0.stable.0"

    # URLs we link to a lot
    env.variables["channel_url"]   = "https://stable-haskell.github.io/ghc/ghcup-multi-target-0.1.0.yaml"
    env.variables["site_url"]      = "https://stable-haskell.github.io/ghc"
    env.variables["repo_url"]      = "https://github.com/stable-haskell/ghc"

    # Per-release artifact base; concrete URLs derive from this + version
    env.variables["release_base"]  = (
        "https://github.com/stable-haskell/ghc/releases/download"
    )
