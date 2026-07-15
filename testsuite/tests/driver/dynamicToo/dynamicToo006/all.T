# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
test('dynamicToo006',
     [normalise_slashes, extra_files(['Main.hs']),
      skip],  # -dynamic-too is deprecated
     makefile_test, ['main'])
