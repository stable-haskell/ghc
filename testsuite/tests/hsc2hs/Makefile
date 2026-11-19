TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

HSC2HS_OPTS = --cc="$(TEST_CC)" $(addprefix --cflag=,$(TEST_CC_OPTS)) --ld=$(TEST_CC) $(addprefix --lflag=,$(TEST_CC_OPTS))

.PHONY: hsc2hs001
hsc2hs001:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: hsc2hs002
hsc2hs002:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: hsc2hs003
hsc2hs003:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: hsc2hs004
hsc2hs004:
	'$(HSC2HS)' $(HSC2HS_OPTS) --cross-compile $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: T3837
T3837:
	LANG=C '$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: T4340
T4340:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: T10272
T10272:
	'$(HSC2HS)' $(HSC2HS_OPTS) --cross-compile $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: T11004
T11004:
	LANG=C '$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: T12504
T12504:
	'$(HSC2HS)' $(HSC2HS_OPTS) T12504/path/to/$@.hsc
ifeq "$(WINDOWS)" "YES"
	grep '{-# LINE 1 "T12504\\\\path\\\\to\\\\$@\.hsc" #-}' T12504/path/to/$@.hs
else
	grep '{-# LINE 1 "T12504/path/to/$@\.hsc" #-}' T12504/path/to/$@.hs
endif

.PHONY: T15758
T15758:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc @T15758.response
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.a.hs

.PHONY: hsc2hs_cross_asm
hsc2hs_cross_asm:
	# 1. Parity across direct / cross / cross-safe:
	LC_ALL=C LANG=C '$(HSC2HS)' $(HSC2HS_OPTS) \
	    --cflag=-fverbose-asm \
	    hsc2hs_cross_asm.hsc -o hsc2hs_cross_asm_direct.hs
	LC_ALL=C LANG=C '$(HSC2HS)' $(HSC2HS_OPTS) \
	    --cflag=-fverbose-asm \
	    --cross-compile hsc2hs_cross_asm.hsc -o hsc2hs_cross_asm_cross.hs
	LC_ALL=C LANG=C '$(HSC2HS)' $(HSC2HS_OPTS) \
	    --cflag=-fverbose-asm \
	    --cross-safe hsc2hs_cross_asm.hsc -o hsc2hs_cross_asm_safe.hs
	diff -u hsc2hs_cross_asm_direct.hs hsc2hs_cross_asm_cross.hs
	diff -u hsc2hs_cross_asm_cross.hs hsc2hs_cross_asm_safe.hs
	# 2. Forced outbound cc response file, include path with a space, -k:
	mkdir -p "rsp inc dir"
	echo "#define RSP_SENTINEL_VAL 424242" > "rsp inc dir/hsc2hs_rsp_inc.h"
	printf '%s\n' \
	    '--cross-compile' \
	    '--cflag=-DTEST_RSP_INCLUDE=1' \
	    '"--cflag=-Irsp inc dir"' \
	    > hsc2hs_in.rsp
	HSC2HS_FORCE_RSP=1 '$(HSC2HS)' $(HSC2HS_OPTS) -k @hsc2hs_in.rsp \
	    hsc2hs_cross_asm.hsc -o hsc2hs_cross_asm_rsp_keep.hs
	test -f hsc2hs_cross_asm_rsp_keep_hsc_make.c.rsp
	HSC2HS_FORCE_RSP=1 '$(HSC2HS)' $(HSC2HS_OPTS) @hsc2hs_in.rsp \
	    hsc2hs_cross_asm.hsc -o hsc2hs_cross_asm_rsp_clean.hs
	test ! -f hsc2hs_cross_asm_rsp_clean_hsc_make.c.rsp
	# 3. Command line over 16 KiB (500 --cflag=-I lines):
	'$(PYTHON)' -c 'open("hsc2hs_huge.rsp","w").write("--cross-compile\n" + "".join("--cflag=-I./dummy_include_padding_dir_%04d\n" % i for i in range(500)))'
	'$(HSC2HS)' $(HSC2HS_OPTS) @hsc2hs_huge.rsp \
	    hsc2hs_cross_asm.hsc -o hsc2hs_cross_asm_huge.hs
	diff -u hsc2hs_cross_asm_cross.hs hsc2hs_cross_asm_huge.hs

.PHONY: hsc2hs_cross_runner
hsc2hs_cross_runner:
	chmod +x hsc2hs_cross_runner_wrapper.sh
	# 1. --no-cross-compile overrides an earlier -x:
	'$(HSC2HS)' $(HSC2HS_OPTS) -x --no-cross-compile hsc2hs_cross_runner.hsc -o hsc2hs_runner_nocross.hs
	# 2. --cross-runner plus repeatable --cross-runner-arg overrides -x:
	'$(HSC2HS)' $(HSC2HS_OPTS) -x \
	    --cross-runner=./hsc2hs_cross_runner_wrapper.sh \
	    --cross-runner-arg=--magic-tag \
	    "--cross-runner-arg=hello world" \
	    hsc2hs_cross_runner.hsc -o hsc2hs_runner_wrapped.hs
	diff -u hsc2hs_runner_nocross.hs hsc2hs_runner_wrapped.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 hsc2hs_runner_wrapped.hs -o hsc2hs_cross_runner_bin
	./hsc2hs_cross_runner_bin
