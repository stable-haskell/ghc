# utils/ghc-throttle/Makefile — Build ghc-throttle and status tool
#
# ghc-throttle is a transparent GHC concurrency limiter.
# POSIX (Linux, macOS, FreeBSD): uses flock() + exec()
# Windows: uses named mutexes + CreateProcess()
#
# This Makefile is for standalone builds in this directory.  The top-level
# Makefile has its own targets that build into _build/bin/.

CC      ?= cc
CFLAGS  ?= -O2 -Wall -Wextra -pedantic

# Platform detection: select source files and executable extension.
ifeq ($(OS),Windows_NT)
  THROTTLE_SRC := ghc-throttle-win.c
  STATUS_SRC   := ghc-throttle-status-win.c
  EXE_EXT      := .exe
else
  THROTTLE_SRC := ghc-throttle.c
  STATUS_SRC   := ghc-throttle-status.c
  EXE_EXT      :=
endif

.PHONY: all clean help

## ── Build ──────────────────────────────────────────────────────────

all: ghc-throttle$(EXE_EXT) ghc-throttle-status$(EXE_EXT)  ## Build both tools

ghc-throttle$(EXE_EXT): $(THROTTLE_SRC)  ## Build the GHC concurrency limiter
	$(CC) $(CFLAGS) -o $@ $<

ghc-throttle-status$(EXE_EXT): $(STATUS_SRC)  ## Build the slot status reporter
	$(CC) $(CFLAGS) -o $@ $<

## ── Maintenance ────────────────────────────────────────────────────

clean:  ## Remove built binaries
	rm -f ghc-throttle ghc-throttle.exe ghc-throttle-status ghc-throttle-status.exe

## ── Help ───────────────────────────────────────────────────────────

help:  ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*## "}; {printf "  \033[36m%-24s\033[0m %s\n", $$1, $$2}'
