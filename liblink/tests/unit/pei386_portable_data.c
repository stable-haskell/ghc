/* Data-only COFF backend controls on a real non-Windows host. The compiler
 * fixture is never initialized or executed. Registry/admission policy is
 * deliberately tested by the integrated all-backends driver instead. */
#define main archive_status_original_main
#include "archive_error_status.c"
#undef main
#include "Linker.c"
#include "linker/PEi386.h"
#include "linker/PEi386Services.h"
#include "ll_format.h"

#if defined(mingw32_HOST_OS)
#error "pei386_portable_data requires real non-Windows process services"
#endif

static void check_unsupported(ll_linker *owner, bool failed, const char *name)
{
    CHECK(failed && owner->error_status == LL_E_UNSUPPORTED,
          "foreign PE service returned success or lost unsupported status");
    CHECK(strstr(owner->error_message, name) != NULL,
          "foreign PE service diagnostic did not identify the required service");
    ll_error_clear(owner);
}

static void test_required_services(ll_linker *owner)
{
    ObjectCode object = {0};
    struct ll_coff_object_info info = {0};
    Section section = {0};
    object.L = owner;
    object.coff_info = &info;
    object.sections = &section;
    object.n_sections = 1;
    uintptr_t base = 47;
    ll_symbol_name *symbol = (ll_symbol_name *)"preserved";
    ll_symbol_addr *module = &section;
    bool empty = true;
    check_unsupported(owner, !ll_pe_host_image_base(owner, &base), "image-base");
    CHECK(base == 47, "unsupported image-base discovery invented an address");
    check_unsupported(owner, ll_pe_host_lookup_dlls(owner, "probe", &object) == NULL,
                      "DLL symbol lookup");
    check_unsupported(owner, ll_pe_host_get_proc(owner, &section, "probe") == NULL,
                      "import-symbol resolution");
    check_unsupported(owner, !ll_pe_host_load_bfd_head(&object, "probe.dll", &symbol,
                                                       &module), "import-library");
    CHECK(strcmp(symbol, "preserved") == 0 && module == &section,
          "unsupported import loading invented a module or changed its symbol");
    check_unsupported(owner, !ll_pe_host_register_unwind(&object, LL_ARCH_X86_64,
                                                         0, &empty), "unwind registration");
    CHECK(info.pdata == NULL && info.xdata == NULL && empty,
          "unsupported unwind registration published ownership or success state");
}

/* Keep the existing native PE residual metadata limitation visible. These
 * three separately allocated payloads already have explicit cleanup in the
 * native Windows ownership fixture; this extraction does not repair them. */
int main(int argc, char **argv)
{
    if (argc != 2) {
        (void)fprintf(stderr, "usage: pei386_portable_data compiler-coff-object\n");
        return 2;
    }
    unsigned char bytes[32768];
    FILE *input = fopen(argv[1], "rb");
    CHECK(input != NULL, "could not open compiler COFF object");
    if (input == NULL) return 1;
    size_t size = fread(bytes, 1, sizeof(bytes), input);
    CHECK(!ferror(input) && feof(input) && size > sizeof(ll_coff_file_header),
          "compiler COFF object is empty, too large, or unreadable");
    CHECK(fclose(input) == 0, "could not close compiler COFF object");
    if (failures != 0) return 1;

    ll_linker owner = {0};
    ll_compat_host_ops ops = make_ops(&host_a);
    ll_compat_config config = {.always_pic = true};
    ll_install_host_for(&owner, &ops, &config);
    ll_init_linker(&owner, 0);
    ll_init_linker_mmap(&owner);
    test_required_services(&owner);

    char *image = LL_MALLOC_FOR(&owner, size, "portable COFF compiler image");
    memcpy(image, bytes, size);
    ObjectCode *object = mkOc(&owner, STATIC_OBJECT, (pathchar *)argv[1], image,
                              (int)size, false, NULL, 0);
    object->format_ops = &ll_format_pei386;
    object->formatName = ll_format_pei386.name;
    CHECK(object->format_ops->sniff(bytes, size), "real COFF table rejected fixture signature");
    if (!object->format_ops->verify(object) || !object->format_ops->get_names(object)) {
        (void)fprintf(stderr, "real COFF data processing failed: %d %s\n",
                      owner.error_status, owner.error_message);
        return 1;
    }
    RtsSymbolInfo *definition = NULL;
    CHECK(ghciLookupSymbolInfo(&owner, "ll_pe_portable_data", &definition),
          "real COFF names did not publish compiler-defined data");
    if (definition != NULL) {
        uint32_t value = 0;
        memcpy(&value, definition->value, sizeof(value));
        CHECK(value == UINT32_C(0x13579bdf) && definition->owner == object,
              "real COFF data bytes or symbol owner changed");
    }

    void *string_table = object->coff_info->str_tab;
    void *names[128] = {0};
    void *relocations[128] = {0};
    CHECK(object->n_sections < 128, "compiler fixture has too many sections");
    for (int i = 0; i < object->n_sections && i < 128; ++i) {
        if (object->sections[i].coff_info != NULL) {
            names[i] = object->sections[i].coff_info->name;
            relocations[i] = object->sections[i].coff_info->relocs;
        }
    }
    int section_count = object->n_sections;
    CHECK(object->format_ops->resolve(object), "ordinary COFF data required a native process service");
    CHECK(owner.error_status == LL_OK, "ordinary COFF data left a process-service error");
    removeOcSymbols(object);
    freeObjectCode(object);
    LL_FREE_FOR(&owner, string_table);
    for (int i = 0; i < section_count && i < 128; ++i) {
        LL_FREE_FOR(&owner, names[i]);
        LL_FREE_FOR(&owner, relocations[i]);
    }
    ll_exit_linker(&owner);
    CHECK(host_a.live == 0 && host_a.alloc_calls == host_a.free_calls,
          "portable COFF fixture leaked ownership beyond recorded metadata limitation");
    CHECK(host_a.fatal_calls == 0, "portable COFF data used fatal callback");
    if (failures != 0) return 1;
    (void)puts("pei386_portable_data: PASS (real verify/names/resolve, required host services reject)");
    return 0;
}
