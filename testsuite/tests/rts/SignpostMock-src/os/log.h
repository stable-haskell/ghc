#pragma once
#include <stddef.h>
typedef void *os_log_t;
#define OS_LOG_CATEGORY_POINTS_OF_INTEREST "PointsOfInterest"
os_log_t os_log_create(const char *, const char *);
