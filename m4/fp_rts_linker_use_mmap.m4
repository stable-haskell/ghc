# FP_RTS_LINKER_USE_MMAP
# ----------------------
# Decide whether the runtime linker uses mmap on the host platform.
AC_DEFUN([FP_RTS_LINKER_USE_MMAP], [
dnl ** Use MMAP in the runtime linker?
dnl --------------------------------------------------------------

case ${HostOS} in
    linux|linux-android|freebsd|dragonfly|netbsd|openbsd|kfreebsdgnu|gnu|solaris2)
        RtsLinkerUseMmap=1
        ;;
    darwin|ios|watchos|tvos)
        RtsLinkerUseMmap=1
        ;;
    *)
        # Windows (which doesn't have mmap) and everything else.
        RtsLinkerUseMmap=0
        ;;
    esac

AC_DEFINE_UNQUOTED([RTS_LINKER_USE_MMAP], [$RtsLinkerUseMmap],
                   [Use mmap in the runtime linker])

])
