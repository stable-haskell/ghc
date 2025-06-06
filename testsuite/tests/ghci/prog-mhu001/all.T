test('prog-mhu001a',
     [extra_files(['e/', 'unitE']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      normalise_slashes,
      req_interp],
     ghci_multiunit_script, [['unitE'], 'prog-mhu001a.script'])
test('prog-mhu001b',
     [extra_files(['e/', 'unitE']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      normalise_slashes,
      req_interp],
     ghci_multiunit_script, [['unitE'], 'prog-mhu001b.script'])
test('prog-mhu001c',
     [extra_files(['e/', 'unitE']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      normalise_slashes,
      req_interp],
     ghci_multiunit_script, [['unitE'], 'prog-mhu001c.script'])
test('prog-mhu001d',
     [extra_files(['e/', 'unitE']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      normalise_slashes,
      req_interp],
     ghci_multiunit_script, [['unitE'], 'prog-mhu001d.script'])
test('prog-mhu001e',
     [extra_files(['e/', 'unitE-main-is']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      normalise_slashes,
      req_interp],
     ghci_multiunit_script, [['unitE-main-is'], 'prog-mhu001e.script'])
test('prog-mhu001f',
     [extra_files(['e/', 'unitE']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      normalise_slashes,
      req_interp],
     ghci_multiunit_script, [['unitE'], 'prog-mhu001f.script'])
