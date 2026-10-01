#!/usr/bin/env python3
"""Generate GHC/Settings/Config.hs like compiler/Setup.hs does.

Usage: gen_settings_config.py <ghc> <ghc-pkg> <output-file>

compiler/Setup.hs (build-type Custom) writes this module at configure
time from `ghc --info` of the boot compiler. cabal buck2 does not run
Custom setups, so a buck2 genrule runs this script instead.
"""
import ast
import subprocess
import sys

ghc, ghc_pkg, out = sys.argv[1:4]

info = dict(ast.literal_eval(subprocess.check_output([ghc, "--info"], text=True)))

# Setup.hs: cStage is hard-coded to 2, even for the stage-1 compiler.
settings = {
    "cBuildPlatformString": info["Host platform"],
    "cHostPlatformString": info["Target platform"],
    "cProjectName": info["Project name"],
    "cBooterVersion": info["Project version"],
    "cStage": "2",
}

# The unit id of the library being built is fixed to "ghc" by
# `-this-unit-id ghc` (compiler/ghc.cabal) and by the buck2 rules.
project_unit_id = "ghc"

# Boot compilers before 9.10 have no ghc-internal package.
r = subprocess.run([ghc_pkg, "field", "ghc-internal", "id"],
                   capture_output=True, text=True)
if r.returncode == 0 and r.stdout.strip():
    ghc_internal_unit_id = r.stdout.split(":", 1)[1].strip()
else:
    ghc_internal_unit_id = "<unavailable>"

def hs_str(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'

lines = [
    "module GHC.Settings.Config",
    "  ( module GHC.Version",
    "  , cBuildPlatformString",
    "  , cHostPlatformString",
    "  , cProjectName",
    "  , cBooterVersion",
    "  , cStage",
    "  , cProjectUnitId",
    "  , cGhcInternalUnitId",
    "  ) where",
    "",
    "import GHC.Prelude.Basic",
    "",
    "import GHC.Version",
    "",
    "cBuildPlatformString :: String",
    "cBuildPlatformString = " + hs_str(settings["cBuildPlatformString"]),
    "",
    "cHostPlatformString :: String",
    "cHostPlatformString = " + hs_str(settings["cHostPlatformString"]),
    "",
    "cProjectName          :: String",
    "cProjectName          = " + hs_str(settings["cProjectName"]),
    "",
    "cBooterVersion        :: String",
    "cBooterVersion        = " + hs_str(settings["cBooterVersion"]),
    "",
    "cStage                :: String",
    "cStage                = show (" + settings["cStage"] + " :: Int)",
    "",
    "cProjectUnitId :: String",
    "cProjectUnitId = " + hs_str(project_unit_id),
    "",
    "cGhcInternalUnitId :: String",
    "cGhcInternalUnitId = " + hs_str(ghc_internal_unit_id),
]

with open(out, "w") as f:
    f.write("\n".join(lines) + "\n")
