/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2025
 *
 * os_signpost integration for macOS Instruments profiling.
 *
 * Provides real-time visualization of GC events, thread lifecycle, and
 * user events in Apple Instruments via the os_signpost API.
 *
 * On non-Darwin platforms all functions are no-ops (empty macros).
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ghcplatform.h"    /* for darwin_HOST_OS */

#if defined(darwin_HOST_OS)

#include <os/log.h>
#include <os/signpost.h>
#include <stdint.h>

/* Initialization and teardown — called from RtsStartup.c */
void initSignposts(void);
void freeSignposts(void);

/* GC interval signposts (begin/end pairs tracked per capability) */
void signpostGcBegin(uint32_t cap_no, uint32_t gen);
void signpostGcEnd(uint32_t cap_no, uint64_t copied, uint64_t slop);

/* Thread lifecycle signposts (point events) */
void signpostThreadCreate(uint32_t cap_no, uint64_t tid);
void signpostThreadRun(uint32_t cap_no, uint64_t tid);
void signpostThreadStop(uint32_t cap_no, uint64_t tid, uint16_t status);

/* User event forwarding (from traceEvent#/traceMarker#) */
void signpostUserMsg(uint32_t cap_no, const char *msg);
void signpostUserMarker(uint32_t cap_no, const char *msg);

#else /* !darwin_HOST_OS */

#define initSignposts()                        /* nothing */
#define freeSignposts()                        /* nothing */
#define signpostGcBegin(cap, gen)              /* nothing */
#define signpostGcEnd(cap, copied, slop)       /* nothing */
#define signpostThreadCreate(cap, tid)         /* nothing */
#define signpostThreadRun(cap, tid)            /* nothing */
#define signpostThreadStop(cap, tid, status)   /* nothing */
#define signpostUserMsg(cap, msg)              /* nothing */
#define signpostUserMarker(cap, msg)           /* nothing */

#endif /* darwin_HOST_OS */
