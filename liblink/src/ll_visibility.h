#pragma once

/* Only the temporary RTS interface crosses the native library boundary. */
#if defined(__GNUC__) || defined(__clang__)
#define LL_FRIEND_API __attribute__((visibility("default")))
#else
#define LL_FRIEND_API
#endif
