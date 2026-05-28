```sh
ghcup config add-release-channel \
  {{ channel_url }}
ghcup install ghc   {{ wasm_version }}
ghcup install cabal {{ cabal_version }}
ghcup set    cabal {{ cabal_version }}
```
