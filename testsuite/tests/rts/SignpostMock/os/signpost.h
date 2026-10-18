#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "log.h"
typedef uint64_t os_signpost_id_t;
#define OS_SIGNPOST_ID_NULL UINT64_C(0)
#define OS_SIGNPOST_ID_EXCLUSIVE UINT64_MAX
bool os_signpost_enabled(os_log_t);
os_signpost_id_t os_signpost_id_generate(os_log_t);
void mockSignpostEmit(unsigned, os_signpost_id_t, const char *, const char *, ...);
#define MOCK_EMIT(kind, log, id, name, ...) do { \
    if (os_signpost_enabled(log)) mockSignpostEmit(kind, id, name, __VA_ARGS__); \
} while (0)
#define os_signpost_interval_begin(log, id, name, ...) MOCK_EMIT(0, log, id, name, __VA_ARGS__)
#define os_signpost_interval_end(log, id, name, ...) MOCK_EMIT(1, log, id, name, __VA_ARGS__)
#define os_signpost_event_emit(log, id, name, ...) MOCK_EMIT(2, log, id, name, __VA_ARGS__)
