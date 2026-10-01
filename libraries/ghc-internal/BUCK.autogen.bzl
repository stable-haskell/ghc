# The two modules of ghc-internal that the compiler prints (its Setup.hs
# does this for a Cabal build): GHC.Internal.Prim and
# GHC.Internal.PrimopWrappers, the targets `cabal buck2` names for
# modules without a source file.

def ghc_internal_autogen_rules(target, ghc):
    native.genrule(
        name = target + "-autogen-GHC.Internal.Prim",
        out = "Prim.hs",
        cmd = ghc + " --print-prim-module > $OUT",
    )
    native.genrule(
        name = target + "-autogen-GHC.Internal.PrimopWrappers",
        out = "PrimopWrappers.hs",
        cmd = ghc + " --print-prim-wrappers-module > $OUT",
    )
