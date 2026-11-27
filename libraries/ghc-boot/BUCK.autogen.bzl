# Autogen rule of ghc-boot, one per stage; see BUCK. `def` is not allowed
# in a BUCK file, hence this .bzl.
#
# Mentioning :BUCK.ghc-pkg-boot.cabal.bzl here stops `cabal buck2 --variant`
# from appending a second call. The base generation writes a stub until the
# variant replaces it (Note [Variant stubs]).
load(":BUCK.stage2.cabal.bzl", STAGE2_GENERATED = "GENERATED", generated_targets_stage2 = "generated_targets")
load(":BUCK.ghc-pkg-boot.cabal.bzl", GHC_PKG_BOOT_GENERATED = "GENERATED", generated_targets_ghc_pkg_boot = "generated_targets")
load("@third-party-haskell//:tools.bzl", "GHC_VERSION")

# The stage-2 rules, once stage 2 is generated (see Note [Variant stubs]
# in cabal-install's Distribution.Client.Buck2.Generate).
def stage2_rules():
    if not STAGE2_GENERATED:
        return
    autogen_rules(target = "ghc-boot-stage2", ghc = "$(exe //buck2-ghc:ghc)")
    generated_targets_stage2()

# ghc-pkg's copy of ghc-boot, built by the boot compiler. The compiler
# plan keeps the unsuffixed :ghc-boot target (filepath 1.5).
def ghc_pkg_boot_rules():
    if not GHC_PKG_BOOT_GENERATED:
        return
    autogen_rules(target = "ghc-boot-ghc-pkg-boot", ghc = "ghc-" + GHC_VERSION)
    generated_targets_ghc_pkg_boot()

def autogen_rules(target, ghc):
    native.genrule(
        name = target + "-autogen-GHC.Platform.Host",
        srcs = ["cabal-buck2/gen_platform_host.py"],
        out = "Host.hs",
        cmd = "python3 $SRCDIR/cabal-buck2/gen_platform_host.py " + ghc + " $OUT",
    )
