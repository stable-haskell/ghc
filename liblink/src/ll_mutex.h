/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Linker-local, non-recursive mutexes.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ll_visibility.h"

#include "ll_platform.h"

#include <stdbool.h>

#if defined(wasm32_HOST_ARCH)
/* The single-threaded Wasm RTS has no native mutex backend. */
#elif defined(_WIN32)
#include <windows.h>
#else
#include <pthread.h>
#endif

typedef struct ll_mutex {
#if defined(wasm32_HOST_ARCH)
    /* Keep the placeholder nonempty through the common initialized field. */
#elif defined(_WIN32)
    SRWLOCK native;
#else
    pthread_mutex_t native;
#endif
    bool initialized;
} ll_mutex;

/* Wasm operations remain no-ops. On native platforms, zero-initialized storage
 * may be closed before initialization. Initialization and shutdown require
 * exclusive access; these functions do not implement once.
 */
void ll_mutex_init(ll_mutex *mutex);
void ll_mutex_close(ll_mutex *mutex);

LL_FRIEND_API void ll_lock_at(ll_mutex *mutex, const char *file, int line);
LL_FRIEND_API void ll_unlock_at(ll_mutex *mutex, const char *file, int line);
void ll_assert_lock_held_at(ll_mutex *mutex, const char *file, int line);

/* Keep lock failure diagnostics attributed to the original call sites. */
#define ll_lock(mutex) ll_lock_at((mutex), __FILE__, __LINE__)
#define ll_unlock(mutex) ll_unlock_at((mutex), __FILE__, __LINE__)
#define ll_assert_lock_held(mutex) \
    ll_assert_lock_held_at((mutex), __FILE__, __LINE__)
