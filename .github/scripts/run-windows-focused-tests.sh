#!/usr/bin/env bash
set -euo pipefail

workspace_root="${GITHUB_WORKSPACE:-$PWD}"
if command -v cygpath >/dev/null 2>&1; then
    workspace_root="$(cygpath -u "$workspace_root")"
fi
cd "$workspace_root"
workspace_root="$(pwd -P)"

dist_bin="$workspace_root/_build/dist/bin"
output_root="$workspace_root/_build/logs/windows-focused"
failed=0

run_testsuite() {
    local label="$1"
    local test_root="$2"
    local way="$3"
    local tests="$4"
    local speed="$5"
    local output_dir="$output_root/$label"
    local make_args=(test)

    mkdir -p "$output_dir"
    if [[ -n "$way" ]]; then
        make_args+=("WAY=$way")
    fi
    if [[ -n "$tests" ]]; then
        make_args+=("TESTS=$tests")
    fi
    if [[ -n "$speed" ]]; then
        make_args+=("SPEED=$speed")
    fi

    TEST_HC="$dist_bin/ghc" \
    GHC_PKG="$dist_bin/ghc-pkg" \
    HP2PS_ABS="$dist_bin/hp2ps" \
    HPC="$dist_bin/hpc" \
    RUNGHC="$dist_bin/runghc" \
    TEST_CC="${CC:-cc}" \
    TEST_CXX="${CXX:-c++}" \
    METRICS_FILE="$output_dir/test-perf.csv" \
    SUMMARY_FILE="$output_dir/test-summary.txt" \
    JUNIT_FILE="$output_dir/test-junit.xml" \
    SKIP_PERF_TESTS=YES \
    THREADS=4 \
    make -C "testsuite/tests/$test_root" "${make_args[@]}" 2>&1 | tee "$output_dir/run.log"
}

run_and_record() {
    local label="$1"
    shift

    if run_testsuite "$label" "$@"; then
        return 0
    else
        local status=$?
        printf 'Focused testsuite selection %s failed with status %s\n' "$label" "$status" >&2
        failed=1
    fi
}

# T1: linker and GHCi linker tests, using each root's registered ways.
run_and_record t1-rts-linker rts/linker '' '' ''
run_and_record t1-ghci-linking ghci/linking '' '' ''

# T2: check the default way and external interpreter across the requested roots.
for test_root in ghci th plugins; do
    run_and_record "t2-${test_root}-normal" "$test_root" normal '' ''
    run_and_record "t2-${test_root}-ext-interp" "$test_root" ext-interp '' ''
done

# T3: unload tests across all available ways, retaining each test's registrations.
run_and_record t3-unload rts/linker '' \
    'linker_unload linker_unload_native linker_unload_multiple_objs T20494 T25191' 0

exit "$failed"
