# Default: skip performance tests (can override with SKIP_PERF_TESTS=NO)
SKIP_PERF_TESTS ?= YES
export SKIP_PERF_TESTS

# --- Test Suite Helper Tool Paths & Flags (Hadrian parity light) ---
# We approximate Hadrian's test invocation without depending on Hadrian.
# Bindist places test tools in _build/bindist/bin (created by the bindist target).
TEST_TOOLS_DIR := _build/bindist/stage2/bin
TEST_GHC       := $(abspath $(TEST_TOOLS_DIR)/ghc$(EXE_EXT))
TEST_GHC_PKG   := $(abspath $(TEST_TOOLS_DIR)/ghc-pkg$(EXE_EXT))
TEST_HP2PS     := $(abspath $(TEST_TOOLS_DIR)/hp2ps$(EXE_EXT))
TEST_HPC       := $(abspath $(TEST_TOOLS_DIR)/hpc$(EXE_EXT))
TEST_RUN_GHC   := $(abspath $(TEST_TOOLS_DIR)/runghc$(EXE_EXT))

# Canonical GHC flags used by the testsuite (mirrors testsuite/mk/test.mk & Hadrian runTestGhcFlags)
CANONICAL_TEST_HC_OPTS = \
	-dcore-lint -dstg-lint -dcmm-lint -no-user-package-db -fno-dump-with-ways \
	-fprint-error-index-links=never -rtsopts -fno-warn-missed-specialisations \
	-fshow-warning-groups -fdiagnostics-color=never -fno-diagnostics-show-caret \
	-Werror=compat -dno-debug-output

# Build timeout utility (needed for some tests) if not already built.
.PHONY: testsuite-timeout
testsuite-timeout:
	$(MAKE) -C testsuite/timeout

# --- Test Target ---

test: _build/bindist/stage2 testsuite-timeout
	@echo "::group::Running tests with THREADS=$(THREADS)" >&2
	# If any required tool is missing, testsuite logic will skip related tests.
	TEST_HC='$(TEST_GHC)' \
	GHC_PKG='$(TEST_GHC_PKG)' \
	HP2PS_ABS='$(TEST_HP2PS)' \
	HPC='$(TEST_HPC)' \
	RUNGHC='$(TEST_RUN_GHC)' \
	TEST_CC='$(CC)' \
	TEST_CXX='$(CXX)' \
	TEST_HC_OPTS='$(CANONICAL_TEST_HC_OPTS)' \
	METRICS_FILE='$(CURDIR)/_build/test-perf.csv' \
	SUMMARY_FILE='$(CURDIR)/_build/test-summary.txt' \
	JUNIT_FILE='$(CURDIR)/_build/test-junit.xml' \
	SKIP_PERF_TESTS='$(SKIP_PERF_TESTS)' \
	THREADS='$(THREADS)' \
	$(MAKE) -C testsuite/tests test
	@echo "::endgroup::"

.PHONY: test
