#pragma once

#include "LinkerInternals.h"

#include "ll_begin_private.h"

void freeNativeCode_POSIX  ( ObjectCode *nc );
void *loadNativeObj_POSIX  ( pathchar *path, char **errmsg );

#include "ll_end_private.h"
