MK_COPY_HEADERS := 1

# --- Source headers ---
# TODO: this is a hack, because of https://github.com/haskell/cabal/issues/11172
#
# $1 = headers
# $2 = source base dirs
# $3 = pkgname
# $4 = ghc-pkg
define copy_headers
  set -e; \
  dest=`$4 field $3 include-dirs | awk '{ print $$2 ; exit }'` ;\
  for h in $1 ; do \
	  mkdir -p "$$dest/`dirname $$h`" ; \
	  for sdir in $2 ; do \
	    if [ -e "$$sdir/$$h" ] ; then \
	      cp -frp "$$sdir/$$h" "$$dest/$$h" ; \
		  break ; \
        fi ; \
	  done ; \
	  [ -e "$$dest/$$h" ] || { echo "Copying $$dest/$$h failed... tried source dirs $2" >&2 ;  exit 2 ; } ; \
  done
endef

RTS_HEADERS_H := \
    rts/Bytecodes.h \
    rts/storage/ClosureTypes.h \
    rts/storage/FunTypes.h \
    stg/MachRegs.h \
    stg/MachRegs/arm32.h \
    stg/MachRegs/arm64.h \
    stg/MachRegs/loongarch64.h \
    stg/MachRegs/ppc.h \
    stg/MachRegs/riscv64.h \
    stg/MachRegs/s390x.h \
    stg/MachRegs/wasm32.h \
    stg/MachRegs/x86.h

define copy_rts_headers_h
  $(call copy_headers,$(RTS_HEADERS_H),rts-headers/include/,rts-headers,$1)
endef

RTS_FS_H := \
    fs.h

define copy_rts_fs_h
  $(call copy_headers,$(RTS_FS_H),rts-fs/,rts-fs,$1)
endef

RTS_H := \
      Cmm.h \
	  HsFFI.h \
	  MachDeps.h \
	  Jumps.h \
	  Rts.h \
	  RtsAPI.h \
	  RtsSymbols.h \
	  Stg.h \
      ghcconfig.h \
	  ghcversion.h \
      rts/ghc_ffi.h \
      rts/Adjustor.h \
      rts/ExecPage.h \
      rts/BlockSignals.h \
      rts/Config.h \
      rts/Constants.h \
      rts/EventLogFormat.h \
      rts/EventLogWriter.h \
      rts/FileLock.h \
      rts/Flags.h \
      rts/ForeignExports.h \
      rts/GetTime.h \
      rts/Globals.h \
      rts/Hpc.h \
      rts/IOInterface.h \
      rts/Libdw.h \
      rts/LibdwPool.h \
      rts/Linker.h \
      rts/Main.h \
      rts/Messages.h \
      rts/NonMoving.h \
      rts/OSThreads.h \
      rts/Parallel.h \
      rts/PrimFloat.h \
      rts/Profiling.h \
      rts/IPE.h \
      rts/PosixSource.h \
      rts/RtsToHsIface.h \
      rts/Signals.h \
      rts/SpinLock.h \
      rts/StableName.h \
      rts/StablePtr.h \
      rts/StaticPtrTable.h \
      rts/TTY.h \
      rts/Threads.h \
      rts/Ticky.h \
      rts/Time.h \
      rts/Timer.h \
      rts/TSANUtils.h \
      rts/Types.h \
      rts/Utils.h \
      rts/prof/CCS.h \
      rts/prof/Heap.h \
      rts/prof/LDV.h \
      rts/storage/Block.h \
      rts/storage/ClosureMacros.h \
      rts/storage/Closures.h \
      rts/storage/Heap.h \
      rts/storage/HeapAlloc.h \
      rts/storage/GC.h \
      rts/storage/InfoTables.h \
      rts/storage/MBlock.h \
      rts/storage/TSO.h \
      stg/DLL.h \
      stg/MiscClosures.h \
      stg/Prim.h \
      stg/Regs.h \
      stg/SMP.h \
      stg/Ticky.h \
      stg/MachRegsForHost.h \
      stg/Types.h

RTS_H_DIRS := \
      rts/ \
      rts/include/

define copy_rts_h
  $(call copy_headers,$(RTS_H),$(RTS_H_DIRS),rts,$1)
endef

RTS_JS_H := \
      HsFFI.h \
	  MachDeps.h \
	  Rts.h \
	  RtsAPI.h \
	  Stg.h \
      ghcconfig.h \
      ghcversion.h \
      stg/MachRegsForHost.h \
      stg/Types.h

define copy_rts_js_h
  $(call copy_headers,$(RTS_JS_H),rts/include/,rts,$1)
endef

HASKELINE_H := \
      win_console.h

define copy_haskeline_h
  $(call copy_headers,$(HASKELINE_H),libraries/haskeline/includes,haskeline,$1)
endef

WIN32_H := \
      HsWin32.h \
      HsGDI.h \
      WndProc.h \
      windows_cconv.h \
      alphablend.h \
      wincon_compat.h \
      winternl_compat.h \
      winuser_compat.h \
      winreg_compat.h \
      tlhelp32_compat.h \
      winnls_compat.h \
      winnt_compat.h \
      namedpipeapi_compat.h

define copy_win32_h
  $(call copy_headers,$(WIN32_H),libraries/Win32/include/,Win32,$1)
endef

GHC_INTERNAL_H := \
      HsBase.h \
      consUtils.h

define copy_ghc_internal_h
  $(call copy_headers,$(GHC_INTERNAL_H),libraries/ghc-internal/include/,ghc-internal,$1)
endef

PROCESS_H := \
      runProcess.h \
      processFlags.h

define copy_process_h
  $(call copy_headers,$(PROCESS_H),libraries/process/include/,process,$1)
endef

BYTESTRING_H := \
      fpstring.h \
      bytestring-cpp-macros.h

define copy_bytestring_h
  $(call copy_headers,$(BYTESTRING_H),libraries/bytestring/include/,bytestring,$1)
endef

TIME_H := \
	HsTime.h

define copy_time_h
  $(call copy_headers,$(TIME_H),libraries/time/lib/include/,time,$1)
endef

UNIX_H := \
    HsUnix.h \
    execvpe.h

define copy_unix_h
  $(call copy_headers,$(UNIX_H),libraries/unix/include/,unix,$1)
endef

define copy_all_stage3_h
  $(call copy_rts_headers_h,$1)
  $(call copy_rts_fs_h,$1)
  $(call copy_rts_h,$1)
  if [ "$2" = "javascript-unknown-ghcjs" ] ; then $(call copy_rts_js_h,$1) ; fi
  $(call copy_ghc_internal_h,$1)
  $(call copy_process_h,$1)
  $(call copy_bytestring_h,$1)
  $(call copy_time_h,$1)
  if [ "$(OS)" = "Windows_NT" ] ; then $(call copy_win32_h,$1) ; else $(call copy_unix_h,$1) ; fi
endef

define copy_all_stage2_h
  $(call copy_all_stage3_h,$1,none)
  $(call copy_haskeline_h,$1)
endef
