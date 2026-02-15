# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
test('dynamicToo002',
     [extra_files(['A.hs', 'B.hs', 'C.hs']),
      skip],  # -dynamic-too is deprecated
     makefile_test, [])
