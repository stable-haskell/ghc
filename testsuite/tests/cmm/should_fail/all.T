test('T24474-cmm-override-g0',
     [cmm_src,
      check_errmsg('GHC-75725'),
      # Relies on GCC, as this is a GCC-related workaround.
      unless(gcc_as_cmmp(), fragile(0))],
     compile_fail,
     ['-optCmmP-g3'])

# See Note [Cmm target attributes] in GHC.Cmm.
# The attribute is per-procedure: the unattributed proc must still be rejected.
# If this ever passes, the attribute has leaked to the whole compilation unit.
# The expected error comes from the X86 NCG; under the llvm ways LLVM compiles
# the unattributed proc happily, and unregisterised gives the C backend's
# rejection instead.  Pin it to the NCG.
test('CmmTargetAttrNoLeak',
     [cmm_src,
      grep_errmsg(r'(256-bit wide vectors require -mavx2)', [1]),
      unless(arch('x86_64') or arch('i386'), skip),
      unless(have_ncg(), skip),
      when(unregisterised(), skip),
      only_ways(['normal'])],
     compile_fail, ['-fasm'])

# An unrecognised feature or attribute must be an error.
test('CmmTargetAttrBadFeature',
     [cmm_src, check_errmsg('unknown target feature')], compile_fail, [''])

test('CmmTargetAttrBadName',
     [cmm_src, check_errmsg('unknown attribute')], compile_fail, [''])

# Backends that cannot honour the attribute must reject it.  Each of these
# only runs on the target that reaches the backend in question.

# Unregisterised builds route Cmm through the C backend.
test('CmmTargetAttrViaC',
     [cmm_src,
      unless(unregisterised(), skip),
      check_errmsg('not supported by the C backend')],
     compile_fail, [''])

test('CmmTargetAttrWasm',
     [cmm_src,
      unless(arch('wasm32'), skip),
      check_errmsg('not supported by the wasm backend')],
     compile_fail, [''])

# The NCG on a target that cannot have these features at all.
test('CmmTargetAttrWrongArch',
     [cmm_src,
      when(arch('x86_64') or arch('i386'), skip),
      when(arch('wasm32'), skip),
      unless(have_ncg(), skip),
      when(unregisterised(), skip),
      omit_ways(llvm_ways),
      check_errmsg('not supported on this architecture')],
     compile_fail, ['-fasm'])
