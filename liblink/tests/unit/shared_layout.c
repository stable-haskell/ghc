/* Compile this TU twice: a backend producer and an installed-header client.
 * Neither private target selection nor all-backends may change shared layout. */
#include "liblink/rts_friend.h"
#include "ll_target.h"

#if !defined(LL_LAYOUT_PRODUCER) && defined(LL_ALL_BACKENDS)
#error "shared-layout client must not inherit private backend flags"
#endif
#include <stddef.h>
#include <stdio.h>

#define OFFSET(type, field) offsetof(type, field)
static const size_t layout[] = {
    sizeof(ObjectCode), _Alignof(ObjectCode), sizeof(Section), _Alignof(Section),
    OFFSET(ObjectCode, format_kind), OFFSET(ObjectCode, info),
    OFFSET(ObjectCode, sections), OFFSET(ObjectCode, proddables),
    OFFSET(ObjectCode, symbol_extras),
    OFFSET(ObjectCode, first_symbol_extra), OFFSET(ObjectCode, n_symbol_extras),
    OFFSET(ObjectCode, symbol_extra_stride), OFFSET(ObjectCode, symbol_extras_origin),
    OFFSET(ObjectCode, bssBegin), OFFSET(ObjectCode, foreign_exports),
    OFFSET(ObjectCode, rw_m32), OFFSET(ObjectCode, rx_m32),
    OFFSET(ObjectCode, shndx_table), OFFSET(ObjectCode, dlopen_handle),
    OFFSET(ObjectCode, nc_ranges), OFFSET(Section, start), OFFSET(Section, info),
    OFFSET(Section, elf_info), OFFSET(Section, macho_info), OFFSET(Section, coff_info)
};

const size_t *shared_layout_producer(size_t *count);
void shared_layout_write(ObjectCode *object, void *payload, ll_elf_word *sentinel);
#ifdef LL_LAYOUT_PRODUCER
const size_t *shared_layout_producer(size_t *count)
{
    *count = sizeof(layout) / sizeof(layout[0]);
    return layout;
}
void shared_layout_write(ObjectCode *object, void *payload, ll_elf_word *sentinel)
{
    object->symbol_extras = payload;
    object->symbol_extras_origin = LL_EXTRAS_SHARED;
    object->first_symbol_extra = 7;
    object->n_symbol_extras = 11;
    object->symbol_extra_stride = 16;
    object->format_kind = LL_OBJECT_FORMAT_ELF;
    object->shndx_table = sentinel;
}
#else
int main(void)
{
    size_t count;
    const size_t *other = shared_layout_producer(&count);
    if (count != sizeof(layout) / sizeof(layout[0])) return 1;
    for (size_t i = 0; i < count; ++i) {
        if (layout[i] != other[i]) {
            (void)fprintf(stderr, "shared_layout: offset %zu differs: %zu / %zu\n",
                          i, layout[i], other[i]);
            return 2;
        }
    }
    ObjectCode object = {0};
    unsigned char payload[16] = {0};
    ll_elf_word sentinel = 0;
    shared_layout_write(&object, payload, &sentinel);
    if ((void *)object.symbol_extras != payload ||
        object.symbol_extras_origin != LL_EXTRAS_SHARED ||
        object.first_symbol_extra != 7 || object.n_symbol_extras != 11 ||
        object.symbol_extra_stride != 16 ||
        object.format_kind != LL_OBJECT_FORMAT_ELF ||
        object.shndx_table != &sentinel) return 3;
    (void)puts("shared_layout: PASS (25 layout values, cross-TU storage exchange)");
    return 0;
}
#endif
