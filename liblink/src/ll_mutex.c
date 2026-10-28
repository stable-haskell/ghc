/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Linker-local, non-recursive mutexes.
 *
 * ---------------------------------------------------------------------------*/

#if !defined(_WIN32) && !defined(_POSIX_C_SOURCE)
#define _POSIX_C_SOURCE 200809L
#endif

#include "ll_autoconf.h"
#include "ll_mutex.h"

#include "ll_compat.h"

#if defined(wasm32_HOST_ARCH)

/* Preserve the existing single-threaded Wasm lock behavior. */
void ll_mutex_init_for(const ll_compat_services *services, ll_mutex *mutex)
{
    (void)services;
    (void)mutex;
}

void ll_mutex_close_for(const ll_compat_services *services, ll_mutex *mutex)
{
    (void)services;
    (void)mutex;
}

void ll_mutex_lock_for_at(const ll_compat_services *services, ll_mutex *mutex,
                          const char *file, int line)
{
    (void)services;
    (void)mutex;
    (void)file;
    (void)line;
}

void ll_mutex_unlock_for_at(const ll_compat_services *services,
                            ll_mutex *mutex, const char *file, int line)
{
    (void)services;
    (void)mutex;
    (void)file;
    (void)line;
}

void ll_assert_lock_held_for_at(const ll_compat_services *services,
                                ll_mutex *mutex, const char *file, int line)
{
    (void)services;
    (void)mutex;
    (void)file;
    (void)line;
}

#else

#include <errno.h>

#define LL_MUTEX_ASSERTS LL_ASSERTS_ENABLED

static void require_mutex(const ll_compat_services *services, ll_mutex *mutex)
{
    if (mutex == NULL) {
        ll_compat_fatal_for(services, "Linker mutex is NULL");
    }
}

static void require_initialized_mutex(const ll_compat_services *services,
                                      ll_mutex *mutex)
{
    require_mutex(services, mutex);
    if (!mutex->initialized) {
        ll_compat_fatal_for(services, "Linker mutex is not initialized");
    }
}

void ll_mutex_init_for(const ll_compat_services *services, ll_mutex *mutex)
{
    require_mutex(services, mutex);
#if defined(_WIN32)
    InitializeSRWLock(&mutex->native);
#else
    int result;
#if LL_MUTEX_ASSERTS
    pthread_mutexattr_t attr;
    result = pthread_mutexattr_init(&attr);
    if (result != 0) {
        ll_compat_fatal_for(services,
                            "Linker mutex attribute initialization failed: %d",
                            result);
    }
    result = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
    if (result != 0) {
        (void)pthread_mutexattr_destroy(&attr);
        ll_compat_fatal_for(services,
                            "Linker mutex attribute setup failed: %d", result);
    }
    result = pthread_mutex_init(&mutex->native, &attr);
    (void)pthread_mutexattr_destroy(&attr);
#else
    result = pthread_mutex_init(&mutex->native, NULL);
#endif
    if (result != 0) {
        ll_compat_fatal_for(services, "Linker mutex initialization failed: %d",
                            result);
    }
#endif
    mutex->initialized = true;
}

void ll_mutex_close_for(const ll_compat_services *services, ll_mutex *mutex)
{
    require_mutex(services, mutex);
    if (!mutex->initialized) {
        return;
    }
#if !defined(_WIN32)
    /* As in the RTS closeMutex, shutdown ignores destruction errors. */
    (void)pthread_mutex_destroy(&mutex->native);
#endif
    mutex->initialized = false;
}

void ll_mutex_lock_for_at(const ll_compat_services *services, ll_mutex *mutex,
                          const char *file, int line)
{
    require_initialized_mutex(services, mutex);
#if defined(LOCK_DEBUG)
    ll_compat_log_for(services, LL_LOG_DEBUG, "ACQUIRE_LOCK(0x%p) %s %d\n",
                      (void *)mutex, file != NULL ? file : "<unknown>", line);
#endif
#if defined(_WIN32)
    (void)file;
    (void)line;
    AcquireSRWLockExclusive(&mutex->native);
#else
    int result = pthread_mutex_lock(&mutex->native);
    if (result != 0) {
        ll_compat_fatal_for(services, "ACQUIRE_LOCK failed (%s:%d): %d",
                            file != NULL ? file : "<unknown>", line, result);
    }
#endif
}

void ll_mutex_unlock_for_at(const ll_compat_services *services,
                            ll_mutex *mutex, const char *file, int line)
{
    require_initialized_mutex(services, mutex);
#if defined(LOCK_DEBUG)
    ll_compat_log_for(services, LL_LOG_DEBUG, "RELEASE_LOCK(0x%p) %s %d\n",
                      (void *)mutex, file != NULL ? file : "<unknown>", line);
#endif
#if defined(_WIN32)
    (void)file;
    (void)line;
    ReleaseSRWLockExclusive(&mutex->native);
#else
    if (pthread_mutex_unlock(&mutex->native) != 0) {
        ll_compat_fatal_for(services, "RELEASE_LOCK: I do not own this lock: %s %d",
                            file != NULL ? file : "<unknown>", line);
    }
#endif
}

void ll_assert_lock_held_for_at(const ll_compat_services *services,
                                ll_mutex *mutex, const char *file, int line)
{
#if LL_MUTEX_ASSERTS && !defined(_WIN32)
    require_initialized_mutex(services, mutex);
    /* ERRORCHECK reports EDEADLK only when this thread already owns the lock.
     * This preserves the existing RTS held-lock assertion.
     */
    int result = pthread_mutex_lock(&mutex->native);
    if (result != EDEADLK) {
        ll_compat_fatal_for(services, "ASSERTION FAILED: file %s, line %u\n",
                            file != NULL ? file : "<unknown>",
                            (unsigned int)line);
    }
#else
    /* SRWLOCK has no ownership query, matching the existing Windows assertion. */
    (void)services;
    (void)mutex;
    (void)file;
    (void)line;
#endif
}

#endif /* !wasm32_HOST_ARCH */

void ll_mutex_init(ll_mutex *mutex)
{
    ll_mutex_init_for(ll_default_services, mutex);
}

void ll_mutex_close(ll_mutex *mutex)
{
    ll_mutex_close_for(ll_default_services, mutex);
}

void ll_lock_at(ll_mutex *mutex, const char *file, int line)
{
    ll_mutex_lock_for_at(ll_default_services, mutex, file, line);
}

void ll_unlock_at(ll_mutex *mutex, const char *file, int line)
{
    ll_mutex_unlock_for_at(ll_default_services, mutex, file, line);
}

void ll_assert_lock_held_at(ll_mutex *mutex, const char *file, int line)
{
    ll_assert_lock_held_for_at(ll_default_services, mutex, file, line);
}
