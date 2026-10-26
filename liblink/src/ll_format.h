/* Private object-format selection. Backend validation remains separate. */
#pragma once

#include "ll_backend.h"

const struct ll_format_ops *ll_format_native(void);
const struct ll_format_ops *ll_format_select(ObjectCode *oc);
int ll_format_require(ObjectCode *oc);

/* Backend-owned, immutable operation tables. Only compiled implementations are
 * referenced by the registry; these declarations do not enable a backend. */
extern const struct ll_format_ops ll_format_elf;
extern const struct ll_format_ops ll_format_macho;
extern const struct ll_format_ops ll_format_pei386;

/* Byte-oriented recognition is shared with the unavailable-backend diagnostic.
 * It does not validate an image or depend on native object-header layouts. */
bool ll_format_sniff_elf(const uint8_t *img, size_t len);
bool ll_format_sniff_macho(const uint8_t *img, size_t len);
bool ll_format_sniff_coff(const uint8_t *img, size_t len);
bool ll_format_sniff_wasm(const uint8_t *img, size_t len);
