/* JSON serialization for one native relocation capture. */

#include "runner_internal.h"

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

static void json_string(const char *text)
{
    const unsigned char *p = (const unsigned char *)text;
    putchar('"');
    while (*p != 0) {
        switch (*p) {
        case '"': fputs("\\\"", stdout); break;
        case '\\': fputs("\\\\", stdout); break;
        case '\b': fputs("\\b", stdout); break;
        case '\f': fputs("\\f", stdout); break;
        case '\n': fputs("\\n", stdout); break;
        case '\r': fputs("\\r", stdout); break;
        case '\t': fputs("\\t", stdout); break;
        default:
            if (*p < 0x20) {
                printf("\\u%04x", (unsigned)*p);
            } else {
                putchar(*p);
            }
            break;
        }
        ++p;
    }
    putchar('"');
}

static void json_hex_bytes(const uint8_t *bytes, size_t size)
{
    static const char digits[] = "0123456789abcdef";
    size_t i;
    putchar('"');
    for (i = 0; i < size; ++i) {
        putchar(digits[bytes[i] >> 4]);
        putchar(digits[bytes[i] & 0x0f]);
    }
    putchar('"');
}

static void json_pointer(const void *pointer)
{
    if (pointer == NULL) {
        fputs("null", stdout);
    } else {
        printf("\"0x%" PRIxPTR "\"", (uintptr_t)pointer);
    }
}

static const char *relocation_name(unsigned type)
{
#if defined(__x86_64__)
    switch (type) {
    case 0: return "X86_64_RELOC_UNSIGNED";
    case 1: return "X86_64_RELOC_SIGNED";
    case 2: return "X86_64_RELOC_BRANCH";
    case 3: return "X86_64_RELOC_GOT_LOAD";
    case 4: return "X86_64_RELOC_GOT";
    case 5: return "X86_64_RELOC_SUBTRACTOR";
    case 6: return "X86_64_RELOC_SIGNED_1";
    case 7: return "X86_64_RELOC_SIGNED_2";
    case 8: return "X86_64_RELOC_SIGNED_4";
    default: return "X86_64_RELOC_UNKNOWN";
    }
#else
    switch (type) {
    case ARM64_RELOC_UNSIGNED: return "ARM64_RELOC_UNSIGNED";
    case ARM64_RELOC_SUBTRACTOR: return "ARM64_RELOC_SUBTRACTOR";
    case ARM64_RELOC_BRANCH26: return "ARM64_RELOC_BRANCH26";
    case ARM64_RELOC_PAGE21: return "ARM64_RELOC_PAGE21";
    case ARM64_RELOC_PAGEOFF12: return "ARM64_RELOC_PAGEOFF12";
    case ARM64_RELOC_GOT_LOAD_PAGE21: return "ARM64_RELOC_GOT_LOAD_PAGE21";
    case ARM64_RELOC_GOT_LOAD_PAGEOFF12: return "ARM64_RELOC_GOT_LOAD_PAGEOFF12";
    case ARM64_RELOC_POINTER_TO_GOT: return "ARM64_RELOC_POINTER_TO_GOT";
    case ARM64_RELOC_ADDEND: return "ARM64_RELOC_ADDEND";
    default: return "ARM64_RELOC_UNKNOWN";
    }
#endif
}

static size_t bounded_name_length(const char *name, size_t limit)
{
    size_t n = 0;
    while (n < limit && name[n] != '\0') {
        ++n;
    }
    return n;
}

static void json_fixed_name(const char *name, size_t size)
{
    char buffer[17];
    size_t n = bounded_name_length(name, size);
    if (n >= sizeof(buffer)) {
        n = sizeof(buffer) - 1;
    }
    memcpy(buffer, name, n);
    buffer[n] = '\0';
    json_string(buffer);
}

static bool is_addend_record(const MachORelocationInfo *relocation)
{
#if defined(__x86_64__)
    (void)relocation;
    return false;
#else
    return relocation->r_type == ARM64_RELOC_ADDEND;
#endif
}

static int relocation_width(const MachORelocationInfo *relocation)
{
    if (is_addend_record(relocation)) {
        return 0;
    }
    if (relocation->r_length > 3) {
        return -1;
    }
    return 1 << relocation->r_length;
}

static int64_t sign_extend_addend(uint32_t value)
{
    value &= UINT32_C(0x00ffffff);
    if ((value & UINT32_C(0x00800000)) != 0) {
        value |= UINT32_C(0xff000000);
    }
    return (int32_t)value;
}

static void emit_relocation(RunnerState *state, ObjectCode *object, size_t section_index,
                            size_t relocation_index, bool *comma)
{
    RunnerSectionCapture *capture = &state->sections[section_index];
    MachORelocationInfo *relocation;
    int width;
    int32_t address;
    bool addend_record;

    if (capture->macho == NULL || capture->section->info == NULL) {
        return;
    }
    if (relocation_index >= capture->macho->nreloc) {
        return;
    }
    relocation = &capture->section->info->relocation_info[relocation_index];
    width = relocation_width(relocation);
    address = relocation->r_address;
    addend_record = is_addend_record(relocation);
    if (width < 0 || address < 0
        || (uint64_t)(uint32_t)address + (uint64_t)width > capture->section->size) {
        fprintf(stderr, "invalid relocation field bounds at section %zu record %zu\n",
                section_index, relocation_index);
        abort();
    }

    if (*comma) putchar(',');
    *comma = true;
    printf("{\"section_index\":%zu,\"relocation_index\":%zu,", section_index,
           relocation_index);
    fputs("\"type\":", stdout);
    json_string(relocation_name(relocation->r_type));
    printf(",\"type_number\":%u,\"P\":", relocation->r_type);
    json_pointer((uint8_t *)capture->section->start + (size_t)address);
    printf(",\"A\":{\"implicit_field_width\":%d,\"implicit_field_bytes\":",
           width);
    if (width == 0) {
        fputs("null", stdout);
    } else {
        json_hex_bytes(capture->loaded + address, (size_t)width);
    }
    if (addend_record) {
        printf(",\"explicit_addend\":%" PRId64, sign_extend_addend(relocation->r_symbolnum));
        if (relocation_index + 1 < capture->macho->nreloc) {
            printf(",\"paired_relocation_index\":%zu", relocation_index + 1);
        } else {
            fputs(",\"paired_relocation_index\":null", stdout);
        }
    } else if (relocation_index > 0
               && is_addend_record(&capture->section->info->relocation_info[relocation_index - 1])) {
        printf(",\"explicit_addend\":%" PRId64 ",\"from_record_index\":%zu",
               sign_extend_addend(capture->section->info->relocation_info[relocation_index - 1].r_symbolnum),
               relocation_index - 1);
    } else {
        fputs(",\"explicit_addend\":null", stdout);
    }
    fputs("},\"before\":", stdout);
    if (width == 0) putchar('"');
    else json_hex_bytes(capture->loaded + address, (size_t)width);
    if (width == 0) putchar('"');
    fputs(",\"after\":", stdout);
    if (width == 0) putchar('"');
    else json_hex_bytes(capture->relocated + address, (size_t)width);
    if (width == 0) putchar('"');
    printf(",\"raw\":{\"r_address\":%d,\"r_symbolnum\":%u,"
           "\"r_pcrel\":%u,\"r_length\":%u,\"r_extern\":%u,\"r_type\":%u,"
           "\"record_bytes\":",
           relocation->r_address, relocation->r_symbolnum, relocation->r_pcrel,
           relocation->r_length, relocation->r_extern, relocation->r_type);
    json_hex_bytes((const uint8_t *)relocation, sizeof(*relocation));
    putchar('}');

    if (!addend_record && relocation->r_extern
        && object->info != NULL
        && relocation->r_symbolnum < object->info->n_macho_symbols) {
        MachOSymbol *symbol = &object->info->macho_symbols[relocation->r_symbolnum];
        void *input_address = NULL;
        const char *input_source = "not-host-supplied";
        size_t host_index;
        if (symbol->name != NULL && strcmp(symbol->name, "___dso_handle") == 0) {
            input_address = state->image_base;
            input_source = "object-image-base";
        } else if (symbol->name != NULL) {
            for (host_index = 0; host_index < state->host_symbol_count; ++host_index) {
                if (strcmp(symbol->name, state->host_symbols[host_index].name) == 0) {
                    input_address = state->host_symbols[host_index].address;
                    input_source = "host-symbol-map";
                    break;
                }
            }
        }
        fputs(",\"S\":{\"observed\":", stdout);
        json_pointer(symbol->addr);
        fputs(",\"input\":", stdout);
        json_pointer(input_address);
        fputs(",\"input_source\":", stdout);
        json_string(input_source);
        putchar('}');
        fputs(",\"symbol\":", stdout);
        if (symbol->name == NULL) putchar('n'), fputs("ull", stdout);
        else json_string(symbol->name);
        fputs(",\"got_slot\":", stdout);
        json_pointer(symbol->got_addr);
    } else {
        fputs(",\"S\":null,\"symbol\":null,\"got_slot\":null", stdout);
    }
    putchar('}');
}

static void emit_got(RunnerState *state)
{
    if (state->got_start == NULL) {
        fputs("null", stdout);
        return;
    }
    printf("{\"address\":");
    json_pointer(state->got_start);
    printf(",\"size\":%zu,\"capture_phase\":\"after-resolve-before-init\",\"bytes\":",
           state->got_size);
    if (state->got_size == 0) fputs("\"\"", stdout);
    else json_hex_bytes(state->got_bytes, state->got_size);
    putchar('}');
}

static void emit_stubs(RunnerState *state)
{
    size_t i;
    bool comma_section = false;

    putchar('[');
    for (i = 0; i < state->section_count; ++i) {
        RunnerSectionCapture *capture = &state->sections[i];
        size_t j;
        if (capture->stub_start == NULL && capture->stub_count == 0) continue;
        if (comma_section) putchar(',');
        comma_section = true;
        printf("{\"section_index\":%zu,\"reserved_start\":", i);
        json_pointer(capture->stub_start);
        printf(",\"reserved_size\":%zu,\"count\":%zu,\"reserved_bytes\":",
               capture->stub_size, capture->stub_count);
        if (capture->stub_size == 0) fputs("\"\"", stdout);
        else json_hex_bytes(capture->stub_bytes, capture->stub_size);
        fputs(",\"capture_phase\":\"after-resolve-before-init\",\"entries\":[",
              stdout);
        for (j = 0; j < capture->stub_count; ++j) {
            if (j != 0) putchar(',');
            fputs("{\"address\":", stdout);
            json_pointer(capture->stubs[j].address);
            fputs(",\"target\":", stdout);
            json_pointer(capture->stubs[j].target);
            printf(",\"flags\":%u}", capture->stubs[j].flags);
        }
        fputs("]}", stdout);
    }
    putchar(']');
}

static void emit_sections(RunnerState *state, ObjectCode *object)
{
    size_t i;
    bool comma_section = false;
    fputs("[", stdout);
    for (i = 0; i < state->section_count; ++i) {
        RunnerSectionCapture *capture = &state->sections[i];
        Section *section = capture->section;
        MachOSection *macho = capture->macho;
        size_t j;
        bool comma_relocation = false;
        if (comma_section) putchar(',');
        comma_section = true;
        printf("{\"index\":%zu,\"kind\":%d,\"alloc\":%d,\"size\":%" PRIu64
               ",\"mapped_start\":", i, (int)section->kind, (int)section->alloc,
               (uint64_t)section->size);
        json_pointer(section->start);
        fputs(",\"vmaddr\":", stdout);
        if (macho == NULL) fputs("null", stdout);
        else printf("\"0x%" PRIx64 "\"", (uint64_t)macho->addr);
        fputs(",\"segment\":", stdout);
        if (macho == NULL) fputs("null", stdout);
        else json_fixed_name(macho->segname, sizeof(macho->segname));
        fputs(",\"name\":", stdout);
        if (macho == NULL) fputs("null", stdout);
        else json_fixed_name(macho->sectname, sizeof(macho->sectname));
        fputs(",\"pre_resolve_bytes\":", stdout);
        if (section->size == 0) fputs("\"\"", stdout);
        else json_hex_bytes(capture->loaded, (size_t)section->size);
        fputs(",\"pre_init_relocated_bytes\":", stdout);
        if (section->size == 0) fputs("\"\"", stdout);
        else json_hex_bytes(capture->relocated, (size_t)section->size);
        fputs(",\"post_init_live_bytes\":", stdout);
        if (section->size == 0) fputs("\"\"", stdout);
        else json_hex_bytes(capture->post_init, (size_t)section->size);
        printf(",\"relocation_count\":%u,\"relocations\":[",
               macho == NULL ? 0 : macho->nreloc);
        if (macho != NULL && section->info != NULL) {
            for (j = 0; j < macho->nreloc; ++j) {
                emit_relocation(state, object, i, j, &comma_relocation);
            }
        }
        fputs("]}", stdout);
    }
    fputs("]", stdout);
}


int runner_json_emit(RunnerState *state)
{
    size_t i;
#if defined(__x86_64__)
    fputs("{\"format\":\"Mach-O\",\"host\":\"x86_64-apple-darwin\",\"input\":", stdout);
#else
    fputs("{\"format\":\"Mach-O\",\"host\":\"arm64-apple-darwin\",\"input\":", stdout);
#endif
    json_string(state->input_path);
    printf(",\"input_size\":%zu,\"object_bytes\":", state->input_size);
    json_hex_bytes(state->input_bytes, state->input_size);
    fputs(",\"image_base\":", stdout);
    json_pointer(state->image_base);
    fputs(",\"host_symbols\":{", stdout);
    for (i = 0; i < state->host_symbol_count; ++i) {
        if (i != 0) putchar(',');
        json_string(state->host_symbols[i].name);
        putchar(':');
        json_pointer(state->host_symbols[i].address);
    }
    putchar('}');
    printf(",\"section_count\":%zu,\"sections\":", state->section_count);
    emit_sections(state, state->object);
    fputs(",\"got\":", stdout);
    emit_got(state);
    fputs(",\"stubs\":", stdout);
    emit_stubs(state);
#if defined(__x86_64__)
    fputs(",\"extras\":{\"address\":", stdout);
    json_pointer(state->object->symbol_extras);
    printf(",\"count\":%lu,\"stride\":%zu,\"bytes\":",
           state->object->n_symbol_extras, state->object->symbol_extra_stride);
    json_hex_bytes((const uint8_t *)state->object->symbol_extras,
                   state->object->n_symbol_extras * state->object->symbol_extra_stride);
    putchar('}');
#endif
    fputs(",\"events_after_resolve\":[", stdout);
    for (i = 0; i < state->event_count; ++i) {
        if (i != 0) putchar(',');
        printf("%u", state->events[i]);
    }
    fputs("]}\n", stdout);
    if (fflush(stdout) != 0) {
        fputs("could not flush JSON output\n", stderr);
        return 0;
    }
    return 1;
}
