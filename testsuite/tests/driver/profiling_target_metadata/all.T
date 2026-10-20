test('profiling_target_metadata',
     [only_ways(['normal']), extra_files(['profiling_target_metadata.py']), req_process],
     makefile_test, [])
