/* Temporary forwarder while the linker still consumes RTS platform config. */
#pragma once

#include "ghcplatform.h"

/* Preserve the relevant early platform setup from Rts.h before standard
 * library headers are included by ll_compat.h. */
#if defined(mingw32_HOST_OS)
#define __MSVCRT__ 1
#endif

#if !defined(_REENTRANT)
#define _REENTRANT 1
#endif
