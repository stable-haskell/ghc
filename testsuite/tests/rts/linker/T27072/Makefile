.PHONY: clean_build_and_run build_and_run clean build

clean_build_and_run:
	$(MAKE) clean
	$(MAKE) build_and_run

build_and_run: build
	./main

clean:
	$(RM) Lib.o main.o main

build: Lib.o main

Lib.o: Lib.c
	# Nested makefile_test makes often see empty $(CC)/$(TEST_CC) even when
	# the outer bindist job exported a compiler. Resolve in the shell.
	@c='$(TEST_CC)'; \
	  if [ -z "$$c" ]; then c='$(CC)'; fi; \
	  if [ -z "$$c" ]; then c=`command -v clang || command -v cc || command -v gcc || true`; fi; \
	  if [ -z "$$c" ] && [ -x /usr/bin/clang ]; then c=/usr/bin/clang; fi; \
	  if [ -z "$$c" ] && [ -x /usr/bin/cc ]; then c=/usr/bin/cc; fi; \
	  if [ -z "$$c" ]; then echo 'T27072: no C compiler found' >&2; exit 1; fi; \
	  "$$c" -c -fPIC Lib.c -o Lib.o

main: main.c
	"$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS)) \
		-no-hs-main -optc-Werror \
		main.c -o main
