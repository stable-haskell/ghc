#pragma once

#include "LinkerInternals.h"

#include "ll_begin_private.h"

void freeNativeCode_POSIX  ( ObjectCode *nc );
void *loadNativeObj_POSIX  ( ll_linker *L, pathchar *path, char **errmsg );

#include "ll_end_private.h"
