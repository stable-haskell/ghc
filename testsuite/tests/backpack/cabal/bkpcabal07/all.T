if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('bkpcabal07',
     [extra_files(['bkpcabal07.cabal', 'Setup.hs', 'M.hs', 'P.hsig']),
      js_broken(22349),
      when(opsys('mingw32'), skip)],
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal07 ' + cleanup])
