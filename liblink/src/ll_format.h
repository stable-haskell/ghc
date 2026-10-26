/* Private object-format selection. Backend validation remains separate. */
#pragma once

#include "ll_backend.h"

const struct ll_format_ops *ll_format_native(void);
const struct ll_format_ops *ll_format_select(ObjectCode *oc);
int ll_format_require(ObjectCode *oc);
