# GHC WASM Cross-Compiler Build Instructions

## Prerequisites

### Nix Only - No ghcup!

All dependencies are provided via Nix. You only need:

```bash
# Verify Nix is installed
nix --version
```

That's it! No ghcup, no manual WASM SDK installation.

## Building WASM Cross-Compiler

### Option A: Build on linux-0 (Recommended)

```bash
./build-wasm-on-linux0.sh
```

This will:
1. Sync source code to linux-0
2. Run the Makefile-based build in nix-shell
3. Takes 1-2 hours

### Option B: Build Locally

```bash
./build-wasm-make.sh
```

Or manually:
```bash
nix-shell
make CABAL=_build/stage0/bin/cabal stage2
make CABAL=_build/stage0/bin/cabal stage3-wasm32-unknown-wasi
```

## Build Process

The build happens in two stages:

### Stage 2: Bootstrap Compiler
```bash
make CABAL=_build/stage0/bin/cabal stage2
```
Builds GHC itself using the GHC 9.8.4 bootstrap compiler from Nix.

**Time:** ~45-60 minutes

### Stage 3: WASM Cross-Compiler
```bash
make CABAL=_build/stage0/bin/cabal stage3-wasm32-unknown-wasi
```
Builds the WASM cross-compiler using stage2.

**Time:** ~30-45 minutes

## Build Output

The WASM cross-compiler will be at:
```
_build/stage3/bin/wasm32-unknown-wasi-ghc
```

## Testing

```bash
# Create a simple program
echo 'main = putStrLn "Hello from WASM!"' > hello.hs

# Compile to WASM (in nix-shell)
nix-shell --run '_build/stage3/bin/wasm32-unknown-wasi-ghc hello.hs -o hello.wasm'

# Run with wasmtime
wasmtime hello.wasm
```

## What Nix Provides

The `shell.nix` environment includes:

- **GHC 9.8.4** - Bootstrap compiler
- **Cabal, Happy, Alex** - Build tools
- **LLVM 18 with Clang** - WASM backend support
- **WASM Cross-Compiler** - From `pkgsCross.wasi32` (Clang 19 with wasilibc)
- **Build essentials** - autoconf, automake, python3, etc.

**100% from nixpkgs** - No ghcup, no manual downloads, no external WASM SDK!

## Build Times

- **Stage 2**: ~45-60 minutes (full GHC bootstrap)
- **Stage 3**: ~30-45 minutes (WASM cross-compiler)
- **Total**: ~1.5-2 hours on linux-0

## About This Implementation

This build uses:
- **Makefile-based build system** (NOT Hadrian - Hadrian is wrong!)
- **100% Nix** (NO ghcup) - provides GHC bootstrap, build tools, and LLVM
- **wasi-sdk 24.0** - official WASM/WASI SDK from WebAssembly/wasi-sdk
- **Alternative __PIC__ fix** - removes guards from `rts/wasm/Wasm.S`
- **Native adjustors** - uses native code instead of libffi (libffi is Emscripten-only)

Our changes implement proper on-demand GlobalRegs compilation as an
alternative to the patch in https://github.com/stable-haskell/ghc/issues/134.

### WASM Toolchain: wasi-sdk 24.0

The shell.nix automatically downloads and sets up wasi-sdk 24.0 which provides:
- `clang` 18.1.8 targeting wasm32-unknown-wasi
- Integrated `wasi-libc` (WASI C library with sysroot)
- LLVM binutils (`llvm-ar`, `llvm-ranlib`, `wasm-ld`)

**Why not nixpkgs pkgsCross.wasi32?**
- nixpkgs LLD 19.1.7 has a bug rejecting valid WebAssembly binary modules
- See error: "archive member '*.c.obj' is neither ET_REL nor LLVM bitcode"
- wasi-sdk 24.0 has a working linker and is the upstream recommended toolchain

The shell.nix creates wrapper scripts (not symlinks) for all WASM tools to ensure
LD_LIBRARY_PATH propagates through Cabal subprocesses to find libLLVM.so.18.1-wasi-sdk.

### libffi Incompatibility with WASI

**Important:** libffi's WebAssembly support is Emscripten-only, NOT compatible with WASI.

The RTS is patched to exclude wasm32 from the libffi-clib dependency (see rts.cabal
lines 596 and 794). The build uses native adjustors instead via the
`--disable-libffi-adjustors` flag passed to ghc-toolchain-bin.

See: https://github.com/libffi/libffi/blob/master/src/wasm/ffi.c#L33
(requires emscripten/emscripten.h, not available in WASI)

## Troubleshooting

### "nix-shell: command not found"

Install Nix:
```bash
curl -L https://nixos.org/nix/install | sh
```

### "make: *** No rule to make target 'stage3-wasm32-unknown-wasi'"

Make sure you're on the `stable-ghc-9.14` branch with the latest Makefile.

### Build fails in stage2

Make sure you're inside `nix-shell` which provides all necessary tools.

### LLVM/Clang version issues

The shell.nix uses LLVM 18. If you need a different version, edit shell.nix.

## Host Plugins During Cross-Compilation

Build and install the plugin with the native GHC from the same compiler build.
Pass that compiler's library directory and the host plugin database to the
cross-compiler:

```bash
host_libdir=$(_build/dist/bin/ghc --print-libdir)
_build/dist/bin/wasm32-unknown-wasi-ghc \
  -plugin-libdir "$host_libdir" \
  -plugin-package-db /path/to/host/plugin-db \
  -plugin-package my-plugin -fplugin My.Plugin Main.hs
```

`-plugin-libdir` supplies the native platform, linker tools and default host
package databases. Host plugins run inside GHC; Template Haskell keeps using the
Wasm interpreter and target packages. Host and target interfaces use separate
caches. The host libraries must match the running compiler's ABI and library
ways. The compiler must have internal interpreter support.

On a native compiler, separate plugin databases also require
`-fexternal-interpreter` for target code. `-clear-plugin-package-dbs` clears the
host database stack; add the host global database again when starting a custom
stack. Plugin database flags never change the target package stack.

The cross regression builds a host plugin and checks the architecture and OS
reported by the running TH splice. It rejects two native compilers passed by
mistake:

```bash
python3 testsuite/tests/plugins/test-host-cross.py \
  --host-ghc _build/dist/bin/ghc \
  --host-ghc-pkg _build/dist/bin/ghc-pkg \
  --cross-ghc _build/dist/bin/wasm32-unknown-wasi-ghc \
  --expected-th-platform wasm32-wasi
```

## Interactive Development

For development work:

```bash
# Enter the nix shell
nix-shell

# Now you have all tools available
ghc --version
cabal --version
llvm-config --version

# Build stages manually
make CABAL=_build/stage0/bin/cabal stage2
make CABAL=_build/stage0/bin/cabal stage3-wasm32-unknown-wasi
```

## References

- Build instructions: https://github.com/stable-haskell/ghc/issues/134
- Stable Haskell GHC: https://github.com/stable-haskell/ghc
