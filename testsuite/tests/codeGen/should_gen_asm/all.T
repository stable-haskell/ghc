is_amd64_codegen = [
    unless(platform('x86_64-unknown-linux'),skip),
    when(unregisterised(), skip),
]

test('memcpy', is_amd64_codegen,  compile_cmp_asm, ['cmm', ''])
test('memcpy-unroll', is_amd64_codegen, compile_cmp_asm, ['cmm', ''])
test('memcpy-unroll-conprop', is_amd64_codegen, compile_cmp_asm, ['cmm', ''])
test('memset-unroll', is_amd64_codegen, compile_cmp_asm, ['cmm', ''])
test('bytearray-memset-unroll', is_amd64_codegen, compile_grep_asm, ['hs', True, ''])
test('bytearray-memcpy-unroll', is_amd64_codegen, compile_grep_asm, ['hs', True, ''])
test('T18137', [when(opsys('darwin'), skip), only_ways(llvm_ways)], compile_grep_asm, ['hs', False, '-fllvm -split-sections'])

# Assembling checks that PIC tables name defined labels. The patterns also
# require a real relative jump table and retained tail-call trampolines.
pic_jump_table_opts = '-O2 -fasm -static -fPIC -fno-external-dynamic-refs -fasm-shortcutting -dasm-lint'
test('pic-jump-table', is_amd64_codegen + [only_ways(['normal'])],
     compile_grep_asm, ['cmm', False, pic_jump_table_opts])
test('pic-jump-table-split',
     is_amd64_codegen + [only_ways(['normal']), extra_files(['pic-jump-table.cmm'])],
     compile_grep_asm, ['cmm', False, pic_jump_table_opts + ' -split-sections'])

test('T24941', [only_ways(['optasm'])], compile, ['-fregs-graph'])
is_aarch64_codegen = [
    unless(arch('aarch64'), skip),
    when(unregisterised(), skip),
]

# AArch64-specific tests
test('aarch64-ushr-subword', is_aarch64_codegen, compile_grep_asm, ['hs', True, '-O'])
test('aarch64-shl-subword', is_aarch64_codegen, compile_grep_asm, ['hs', True, '-O'])
test('aarch64-sxtw', is_aarch64_codegen, compile_grep_asm, ['cmm', True, ''])
test('aarch64-sxth-mul2', is_aarch64_codegen, compile_grep_asm, ['cmm', True, ''])

# Assemble as well as inspecting DWARF. The floating point unwind rules exercise
# DWARF register numbers above the compact opcode range. Nine C arguments also
# require an adjusted machine SP, independently of the Haskell stack's CFA.
test('aarch64-dwarf-unwind',
     is_aarch64_codegen + [only_ways(['normal']), unless(opsys('linux') or opsys('darwin'), skip)],
     compile_grep_asm, ['cmm', False, '-fasm -g3 -dasm-lint -dcmm-lint'])
