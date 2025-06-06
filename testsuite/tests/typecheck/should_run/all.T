# args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# -----------------------------------------------------------------------------
# Pick an arbitrary few to run when 'fast' is on

test('tcrun001', normal, compile_and_run, [''])
test('tcrun002', normal, compile_and_run, [''])
test('tcrun003', normal, compile_and_run, [''])
test('tcrun004', normal, compile_and_run, [''])
test('tcrun005', normal, compile_and_run, [''])
test('Defer01', normal, compile_and_run, [''])
test('TcNullaryTC', [], compile_and_run, [''])
test('TcCoercible', [], compile_and_run, [''])

# -----------------------------------------------------------------------------
# Skip everything else if fast is on

setTestOpts(when(fast(), skip))

test('tcrun006', normal, compile_and_run, [''])
test('tcrun008', normal, compile_and_run, [''])
test('tcrun009', normal, compile_and_run, [''])
test('tcrun010', normal, compile_and_run, [''])
test('tcrun011', normal, compile_and_run, [''])
test('tcrun012', normal, compile_and_run, [''])
test('tcrun013', normal, compile_and_run, [''])
test('tcrun014', normal, compile_and_run, [''])
test('tcrun015', normal, compile_and_run, ['-O'])
test('tcrun016', normal, compile_and_run, [''])
test('tcrun017', normal, compile_and_run, [''])
test('tcrun018', normal, compile_and_run, [''])
test('tcrun019', normal, compile_and_run, [''])
test('tcrun020', normal, compile_and_run, [''])
test('tcrun021', normal, compile_and_run, ['-package containers'])
test('tcrun022', omit_ways(['hpc']), compile_and_run, ['-O'])
test('tcrun023', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, ['-O'])
test('tcrun024', normal, compile_and_run, ['-O'])
test('tcrun025', [extra_files(['TcRun025_B.hs'])], multimod_compile_and_run, ['tcrun025', ''])
test('tcrun026', normal, compile_and_run, [''])
test('tcrun027', normal, compile_and_run, [''])
test('tcrun028', normal, compile_and_run, [''])
test('tcrun029', normal, compile_and_run, [''])
test('tcrun030', normal, compile_and_run, [''])
test('tcrun031', normal, compile_and_run, [''])
test('tcrun032', normal, compile_and_run, [''])
test('tcrun033', normal, compile_and_run, [''])
test('tcrun034', normal, compile_and_run, [''])
test('tcrun035', normal, compile_and_run, [''])
test('tcrun036', normal, compile_and_run, [''])
test('tcrun037', normal, compile_and_run, [''])

test('tcrun038', [extra_files(['TcRun038_B.hs'])], multimod_compile_and_run, ['tcrun038', ''])

test('tcrun039', normal, compile_and_run, [''])
test('tcrun040', normal, compile_and_run, [''])
test('tcrun041', normal, compile_and_run, [''])
test('tcrun042', normal, compile_fail, [''])
test('tcrun043', normal, compile_and_run, [''])
test('tcrun044', normal, compile_and_run, [''])
test('tcrun045', normal, compile_fail, [''])
test('tcrun046', normal, compile_and_run, [''])
test('tcrun047', normal, compile_and_run, [''])
test('tcrun048', normal, compile_and_run, [''])
test('tcrun049', normal, compile_and_run, [''])
test('tcrun050', normal, compile_and_run, [''])
test('tcrun051', normal, compile_and_run, [''])

test('church', normal, compile_and_run, [''])
test('testeq2', normal, compile_and_run, [''])
test('T1624', normal, compile_and_run, [''])
test('IPRun', normal, compile_and_run, [''])
test('IPLocation', normal, compile_and_run, [''])
test('T10845', normal, compile_and_run, [''])
test('T10846', normal, compile_and_run, [''])
test('T16646', normal, compile_and_run, [''])

# Support files for T1735 are in directory T1735_Help/
test('T1735', normal, multimod_compile_and_run, ['T1735',''])

# The following two tests no longer compile
# See Note [Inferring principal types] in Ghc.Tc.Solver
test('T3731', normal, compile_fail, [''])
test('T3731_simple', normal, compile_fail, [''])

test('T3731-short', normal, compile_and_run, [''])

test('T3500a', normal, compile_and_run, [''])
test('T3500b', normal, compile_and_run, [''])
test('T4809', normal, compile_and_run, [''])
test('T2722', normal, compile_and_run, [''])
test('mc17', normal, compile_and_run, [''])
test('T5759', normal, compile_and_run, [''])
test('T5570', normal, compile_and_run, [''])
test('T5573a', normal, compile_and_run, [''])
test('T5573b', normal, compile_and_run, [''])
test('T7023', normal, compile_and_run, [''])
test('T7126', normal, compile_and_run, [''])
test('T6117', normal, compile_and_run, [''])
test('T5751', normal, compile_and_run, [''])
test('T5913', normal, compile_and_run, [''])
test('T7748', normal, compile_and_run, [''])
test('T7861',
     [expect_broken(18467),
      omit_ghci, # fails during compilation phase, remove after !10563
      omit_ways(['debug']),
      exit_code(1)], compile_and_run, [''])
test('TcTypeNatSimpleRun', normal, compile_and_run, [''])
test('TcTypeSymbolSimpleRun', normal, compile_and_run, [''])
test('T8119', normal, ghci_script, ['T8119.script'])
test('T8492', normal, compile_and_run, [''])
test('T8739', normal, compile_and_run, [''])
test('T9497a-run', [exit_code(1)], compile_and_run, ['-fdefer-typed-holes'])
test('T9497b-run', [exit_code(1)], compile_and_run, ['-fdefer-typed-holes -fno-warn-typed-holes'])
test('T9497c-run', [exit_code(1)], compile_and_run, ['-fdefer-type-errors -fno-warn-typed-holes'])
test('T9858c', normal, compile_and_run, [''])
test('T9858d', normal, compile_and_run, [''])
test('T10284', exit_code(1), compile_and_run, [''])
test('T11049', exit_code(1), compile_and_run, [''])
test('T11230', normal, compile_and_run, [''])
test('TypeOf', normal, compile_and_run, [''])
test('TypeRep', normal, compile_and_run, [''])
test('T11120', normal, compile_and_run, [''])
test('KindInvariant', normal, ghci_script, ['KindInvariant.script'])

test('StrictPats', normal, compile_and_run, [''])
test('T12809', normal, compile_and_run, [''])
test('EtaExpandLevPoly', normal, compile_and_run, [''])

test('TestTypeableBinary', normal, compile_and_run, [''])
test('Typeable1', normal, compile_fail, ['-Werror'])
test('TypeableEq', normal, compile_and_run, [''])
test('T13435', normal, compile_and_run, [''])
test('T11647', normal, ghci_script, ['T11647.script'])
test('T11715', exit_code(1), compile_and_run, [''])
test('T13594a', normal, ghci_script, ['T13594a.script'])

# GHCi exits with code zero due to deferred type errors
test('T13838', [exit_code(1), omit_ghci], compile_and_run, ['-fdefer-type-errors'])
test('T14218', normal, compile_and_run, [''])
test('T14236', normal, compile_and_run, [''])
test('T14925', normal, compile_and_run, [''])
test('T14341', normal, compile_and_run, [''])
test('T16453M0', extra_files(['T16453T.hs']), compile_and_run, [''])
test('T16453M1', [extra_files(['T16453T.hs']), omit_ghci], compile_and_run, [''])
test('T16453M2', extra_files(['T16453T.hs']), compile_and_run, [''])
test('T16453M3', extra_files(['T16453T.hs']), compile_and_run, [''])
test('T16453M4', [extra_files(['T16453T.hs']), omit_ghci], compile_and_run, [''])
test('T16453M5', extra_files(['T16453T.hs']), compile_and_run, [''])
test('T16453M6', extra_files(['T16453T.hs']), compile_and_run, ['-main-is foo'])
test('UnliftedNewtypesRun', normal, compile_and_run, [''])
test('UnliftedNewtypesFamilyRun', normal, compile_and_run, [''])
test('UnliftedNewtypesDependentFamilyRun', normal, compile_and_run, [''])
test('UnliftedNewtypesIdentityRun', normal, compile_and_run, [''])
test('UnliftedNewtypesCoerceRun', normal, compile_and_run, [''])
test('LevPolyResultInst', normal, compile_and_run, [''])
test('T17104', normal, compile_and_run, [''])
test('T18627', normal, compile_and_run, ['-O'])  # Optimisation shows up the bug
test('T19397M0', extra_files(['T19397S.hs']), compile_and_run, [''])
test('T19397M1', extra_files(['T19397S.hs']), compile_and_run, [''])
test('T19397M2', extra_files(['T19397S.hs']), compile_and_run, ['-main-is foo'])
test('T19397M3', extra_files(['T19397S.hs']), compile_and_run, ['-main-is foo'])
test('T19397M4', extra_files(['T19397S.hs']), compile_and_run, ['-main-is foo'])
test('T19667', normal, compile_and_run, ['-fhpc'])
test('T20768', normal, compile_and_run, [''])
test('T22510', normal, compile_and_run, [''])
test('T21973a', [exit_code(1)], compile_and_run, [''])
test('T21973b', normal, compile_and_run, [''])
test('T23761', normal, compile_and_run, [''])
test('T25529', normal, compile_and_run, [''])
test('T23761b', normal, compile_and_run, [''])
test('T17594e', normal, compile_and_run, [''])
test('T25998', normal, compile_and_run, [''])

# Tests for expanding do before typechecking (Impredicative + RebindableSyntax)
test('T18324', normal, compile_and_run, [''])
test('T15598', normal, compile_and_run, [''])
test('T22086', normal, compile_and_run, [''])
test('T24411', normal, compile_and_run, [''])
test('DefaultExceptionContext', normal, compile_and_run, [''])
