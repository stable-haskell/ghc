```sh
ghcup config add-release-channel \
  {{ channel_url }}
ghcup install ghc   {{ ghc_version }}
ghcup set     ghc   {{ ghc_version }}
ghcup install cabal {{ cabal_version }}
ghcup set     cabal {{ cabal_version }}
```
