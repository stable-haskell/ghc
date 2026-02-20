{
  description = "GHC WASM Cross-Compiler Build Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta.url = "git+https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta.git";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Use wasi-sdk from ghc-wasm-meta with libffi-wasm pre-integrated
        wasi-sdk = ghc-wasm-meta.packages.${system}.wasi-sdk;

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Bootstrap GHC compiler
            haskell.compiler.ghc984
            haskell.packages.ghc984.cabal-install
            haskell.packages.ghc984.happy
            haskell.packages.ghc984.alex

            # Build tools
            autoconf
            automake
            python3
            m4

            # For WASM - llvm and clang with wasm support (native tools)
            llvm_18
            clang_18

            # Additional tools
            git
            gnumake
            which
            curl

            # NOTE: wasi-sdk is intentionally NOT in buildInputs to prevent nix
            # from setting CC/LD/AR to the WASM cross-compiler globally.
            # It's accessed exclusively via $WASI_SDK_DIR set in shellHook.
          ];

          shellHook = ''
            echo "GHC WASM Cross-Compiler Build Environment"
            echo "=========================================="
            echo ""
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version | head -1)"
            echo "LLVM version: $(llvm-config --version)"
            echo "Clang version: $(clang --version | head -1)"
            echo ""

            # wasi-sdk from ghc-wasm-meta includes libffi-wasm
            export WASI_SDK_DIR="${wasi-sdk}"

            # Create short-name wrappers for WASM toolchain
            # GHC Makefile expects wasm32-wasi-* but wasi-sdk provides full names
            WASM_BIN_DIR="$PWD/.nix-wasm-bin"
            mkdir -p "$WASM_BIN_DIR"

            # Create wrapper scripts for WASM toolchain
            cat > "$WASM_BIN_DIR/wasm32-wasi-clang" <<'EOF'
#!/bin/sh
exec "${wasi-sdk}/bin/clang" "$@"
EOF
            chmod +x "$WASM_BIN_DIR/wasm32-wasi-clang"

            # GHC's WASM LLVM backend uses llc/opt/llvm-as to compile LLVM IR to WASM
            # assembly. We must use wasi-sdk's LLVM tools (version 21) since they match
            # the assembler (clang 21). Using nixpkgs' LLVM 18 tools with clang 21's
            # assembler causes errors: ".size directive ignored for function symbols"
            cat > "$WASM_BIN_DIR/llc" <<'EOF'
#!/bin/sh
exec "${wasi-sdk}/bin/llc" "$@"
EOF
            chmod +x "$WASM_BIN_DIR/llc"

            cat > "$WASM_BIN_DIR/opt" <<'EOF'
#!/bin/sh
exec "${wasi-sdk}/bin/opt" "$@"
EOF
            chmod +x "$WASM_BIN_DIR/opt"

            cat > "$WASM_BIN_DIR/llvm-as" <<'EOF'
#!/bin/sh
exec "${wasi-sdk}/bin/llvm-as" "$@"
EOF
            chmod +x "$WASM_BIN_DIR/llvm-as"

            cat > "$WASM_BIN_DIR/wasm32-wasi-clang++" <<'EOF'
#!/bin/sh
exec "${wasi-sdk}/bin/clang++" "$@"
EOF
            chmod +x "$WASM_BIN_DIR/wasm32-wasi-clang++"

            cat > "$WASM_BIN_DIR/wasm-ld" <<'EOF'
#!/bin/sh
exec "${wasi-sdk}/bin/wasm-ld" "$@"
EOF
            chmod +x "$WASM_BIN_DIR/wasm-ld"

            # NOTE: Do NOT create an 'ld' wrapper here.
            # ghc-toolchain-bin auto-detects the native linker for stage1 settings.
            # If 'ld' is the WASM linker, it breaks 'ld -r' tests for native compilation.
            # The WASM settings file explicitly uses '--ld wasm-ld', not 'ld'.

            cat > "$WASM_BIN_DIR/wasm32-wasi-ar" <<'EOF'
#!/bin/sh
exec "${wasi-sdk}/bin/llvm-ar" "$@"
EOF
            chmod +x "$WASM_BIN_DIR/wasm32-wasi-ar"

            cat > "$WASM_BIN_DIR/wasm32-wasi-ranlib" <<'EOF'
#!/bin/sh
exec "${wasi-sdk}/bin/llvm-ranlib" "$@"
EOF
            chmod +x "$WASM_BIN_DIR/wasm32-wasi-ranlib"

            # Add WASM wrappers to PATH
            export PATH="$WASM_BIN_DIR:$PATH"

            echo "WASM toolchain from ghc-wasm-meta wasi-sdk:"
            echo "  - wasm32-wasi-clang (wrapped)"
            echo "  - wasm32-wasi-clang++ (wrapped)"
            echo "  - wasm-ld (wrapped, native ld is NOT overridden)"
            echo "  - wasm32-wasi-ar, wasm32-wasi-ranlib (wrapped)"
            echo "  - llc, opt, llvm-as (wrapped from wasi-sdk LLVM 21)"
            echo "  - Sysroot: ${wasi-sdk}/share/wasi-sysroot"
            echo "  - Includes libffi-wasm pre-integrated"
            echo ""

            echo "To build WASM cross-compiler:"
            echo "  make CABAL=_build/stage0/bin/cabal stage2"
            echo "  make CABAL=_build/stage0/bin/cabal stage3-wasm32-unknown-wasi"
            echo ""
          '';
        };
      }
    );
}
