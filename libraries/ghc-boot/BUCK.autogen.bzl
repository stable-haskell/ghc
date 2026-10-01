# Autogen rule of ghc-boot, one per stage; see BUCK. `def` is not allowed
# in a BUCK file, hence this .bzl.
load(":BUCK.stage2.cabal.bzl", "GENERATED", generated_targets_stage2 = "generated_targets")

# The stage-2 rules, once stage 2 is generated (see Note [Variant stubs]
# in cabal-install's Distribution.Client.Buck2.Generate).
def stage2_rules():
    if not GENERATED:
        return
    autogen_rules(target = "ghc-boot-stage2", ghc = "$(exe //buck2-ghc:ghc)")
    generated_targets_stage2()

def autogen_rules(target, ghc):
    native.genrule(
        name = target + "-autogen-GHC.Platform.Host",
        srcs = ["cabal-buck2/gen_platform_host.py"],
        out = "Host.hs",
        cmd = "python3 $SRCDIR/cabal-buck2/gen_platform_host.py " + ghc + " $OUT",
    )
