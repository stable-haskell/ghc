# Autogen rule of ghc-boot, one per stage; see BUCK. `def` is not allowed
# in a BUCK file, hence this .bzl.

def autogen_rules(target, ghc):
    native.genrule(
        name = target + "-autogen-GHC.Platform.Host",
        srcs = ["cabal-buck2/gen_platform_host.py"],
        out = "Host.hs",
        cmd = "python3 $SRCDIR/cabal-buck2/gen_platform_host.py " + ghc + " $OUT",
    )
