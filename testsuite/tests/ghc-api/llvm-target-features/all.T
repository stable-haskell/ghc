from testutil import getStdout

# This tests the LLVM feature-list API without invoking LLVM tools.  The API
# is only exposed when the ghc library was built with the LLVM backend.
have_llvm_feature_api = bool(getStdout([config.ghc_pkg, 'find-module',
    'GHC.Driver.Config.CmmToLlvm', '--simple-output', '--no-user-package-db']).strip())

test('LlvmTargetFeatures',
     [unless(have_llvm_feature_api, skip),
      extra_run_opts(f'"{config.libdir}"')],
     compile_and_run, ['-package ghc'])
