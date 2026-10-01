# Autogen rules of the `ghc` library (compiler/), one set per stage; see
# BUCK. `def` is not allowed in a BUCK file, hence this .bzl.

# `ghc` is the compiler that builds the stage: a name on $PATH for stage 1
# (boot compiler), the //buck2-ghc wrappers for stage 2 (exec deps).
# `ghc_pkg_or_internal` names the ghc-internal of that stage for
# gen_settings_config.py: "--ghc-pkg <ghc-pkg>" to look it up in the
# compiler's package db (stage 1), or "--ghc-internal-unit-id <uid>".
def autogen_rules(target, ghc, ghc_pkg_or_internal, unit_id):
    # Setup.hs: deriveConstants --gen-haskell-type -o <out> --target-os <os>
    # with <os> the `target os` entry of `ghc --info` (e.g. OSLinux).
    # Backquotes, not $(...): buck2 expands $(...) itself.
    native.genrule(
        name = target + "-autogen-GHC.Platform.Constants",
        out = "Constants.hs",
        cmd = "$(exe //utils/deriveConstants:deriveConstants) --gen-haskell-type -o $OUT --target-os \"`" + ghc + " --info | sed -n 's/.*(\"target os\",\"\\\\([^\"]*\\\\)\").*/\\\\1/p'`\"",
    )
    native.genrule(
        name = target + "-autogen-GHC.Settings.Config",
        srcs = ["cabal-buck2/gen_settings_config.py"],
        out = "Config.hs",
        cmd = "python3 $SRCDIR/cabal-buck2/gen_settings_config.py --ghc " + ghc + " --unit-id " + unit_id + " " + ghc_pkg_or_internal + " -o $OUT",
    )
