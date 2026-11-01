# The two modules of ghc-internal that the compiler prints (its Setup.hs
# does this for a Cabal build): GHC.Internal.Prim and
# GHC.Internal.PrimopWrappers, the targets `cabal buck2` names for
# modules without a source file.
load(":BUCK.stage2.cabal.bzl", "GENERATED", generated_targets_stage2 = "generated_targets")

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

# The stage-2 rules, once stage 2 is generated (see Note [Variant stubs]
# in cabal-install's Distribution.Client.Buck2.Generate).
def stage2_rules():
    if not GENERATED:
        return
    ghc_internal_autogen_rules(target = "ghc-internal-stage2", ghc = "$(exe //buck2-ghc:ghc)")
    generated_targets_stage2()
