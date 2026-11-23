/* (c) The GHC Team, 2026
 * macOS Instruments signposts. Other targets do not evaluate the arguments.
 */
#pragma once

#include "ghcplatform.h"
#include <stdint.h>

#if defined(darwin_HOST_OS)
void initSignposts(void);
/* The GC leader keeps the interval ID on its own stack: no capability-indexed
 * allocation, resizing, shared interval state, or shutdown lifetime is needed. */
uint64_t signpostGcBegin(uint32_t cap_no, uint32_t gen);
void signpostGcEnd(uint64_t id, uint64_t copied, uint64_t slop);
void signpostThreadCreate(uint32_t cap_no, uint64_t tid);
void signpostThreadRun(uint32_t cap_no, uint64_t tid);
void signpostThreadStop(uint32_t cap_no, uint64_t tid, uint16_t status);
void signpostUserMsg(uint32_t cap_no, const char *msg);
void signpostUserMarker(uint32_t cap_no, const char *msg);
#else
#define initSignposts()                         /* nothing */
#define signpostGcBegin(cap, gen)               UINT64_C(0)
#define signpostGcEnd(id, copied, slop)          /* nothing */
#define signpostThreadCreate(cap, tid)          /* nothing */
#define signpostThreadRun(cap, tid)             /* nothing */
#define signpostThreadStop(cap, tid, status)    /* nothing */
#define signpostUserMsg(cap, msg)               /* nothing */
#define signpostUserMarker(cap, msg)            /* nothing */
#endif
