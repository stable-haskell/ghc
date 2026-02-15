# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
test('dynamicToo001boot',
     [extra_files(['A.hs', 'B1.hs-boot', 'B2.hs-boot', 'C.hs']),
      skip],  # -dynamic-too is deprecated
     makefile_test, [])
