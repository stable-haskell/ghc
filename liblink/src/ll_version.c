#if !defined(_WIN32) && (defined(__GNUC__) || defined(__clang__))
#define LL_VERSION_HIDDEN __attribute__((visibility("hidden")))
#else
#define LL_VERSION_HIDDEN
#endif

const char ll_package_version[] LL_VERSION_HIDDEN = "0.1.0.0";
