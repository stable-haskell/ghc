# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
test('dynamicToo004',
     [extra_files(['Setup.hs', 'pkg1/', 'pkg1dyn/', 'pkg2/', 'prog.hs']),
      skip],  # -dynamic-too is deprecated
     makefile_test, [])
