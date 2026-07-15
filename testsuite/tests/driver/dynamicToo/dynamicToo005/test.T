# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
test('dynamicToo005',
     [extra_files(['dynamicToo005.bkp']),
      skip],  # -dynamic-too is deprecated
     makefile_test, [])
