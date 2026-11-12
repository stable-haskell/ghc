/* Exercise duplicate COFF COMDAT definitions without executing object code. */
#define main archive_status_original_main
#include "archive_error_status.c"
#undef main
#include "Linker.c"
#include "linker/PEi386.h"
#include "RtsSymbolInfo.h"
#include "ll_format.h"

typedef struct {
    ObjectCode *object;
    void *strings;
    void *names[128];
    void *relocs[128];
    int section_count;
} Loaded;

static bool load_coff(ll_linker *owner, const char *path, Loaded *loaded)
{
    FILE *input = fopen(path, "rb");
    if (!input || fseek(input, 0, SEEK_END) != 0) return false;
    long length = ftell(input);
    if (length <= 0 || length > 1024 * 1024 || fseek(input, 0, SEEK_SET) != 0)
        return false;
    char *image = LL_MALLOC_FOR(owner, (size_t)length, "COMDAT image");
    if (fread(image, 1, (size_t)length, input) != (size_t)length || fclose(input) != 0)
        return false;
    ObjectCode *object = mkOc(owner, STATIC_OBJECT, (pathchar *)path, image,
                              (int)length, false, NULL, 0);
    if (!object || !ll_init_object(object) || !object->format_ops->verify(object) ||
        !object->format_ops->get_names(object)) return false;
    loaded->object = object;
    loaded->strings = object->coff_info->str_tab;
    loaded->section_count = object->n_sections;
    if (loaded->section_count > 128) return false;
    for (int i = 0; i < loaded->section_count; ++i) {
        if (object->sections[i].coff_info) {
            loaded->names[i] = object->sections[i].coff_info->name;
            loaded->relocs[i] = object->sections[i].coff_info->relocs;
        }
    }
    return object->format_ops->resolve(object);
}

static void release_coff(ll_linker *owner, Loaded *loaded)
{
    removeOcSymbols(loaded->object);
    freeObjectCode(loaded->object);
    LL_FREE_FOR(owner, loaded->strings);
    for (int i = 0; i < loaded->section_count; ++i) {
        LL_FREE_FOR(owner, loaded->names[i]);
        LL_FREE_FOR(owner, loaded->relocs[i]);
    }
}

int main(int argc, char **argv)
{
    if (argc != 3) return 2;
    for (int noexec = 0; noexec <= 1; ++noexec) {
        ll_linker owner = {0};
        ll_compat_host_ops ops = make_ops(&host_a);
        ll_compat_config config = {.always_pic = true,
            .flags = noexec ? LL_CFG_NO_EXEC | LL_CFG_ALL_FORMATS : 0,
            .noexec_base = noexec ? UINT64_C(0x10000000) : 0};
        ll_install_host_for(&owner, &ops, &config);
        ll_init_linker(&owner, 0);
        ll_init_linker_mmap(&owner);
        ll_mutex_lock_for(&owner, &owner.linker_mutex);
        Loaded first = {0}, second = {0};
        if (!load_coff(&owner, argv[1], &first) ||
            !load_coff(&owner, argv[2], &second)) {
            (void)fprintf(stderr, "COMDAT load failed: %d %s\n",
                          owner.error_status, owner.error_message);
            return 1;
        }
        RtsSymbolInfo *symbol = NULL;
        CHECK(ghciLookupSymbolInfo(&owner, "ll_fixture_comdat", &symbol),
              "COMDAT symbol missing");
        CHECK(symbol != NULL && symbol->owner == first.object,
              "COMDAT duplicate changed the selected owner");
        if (symbol != NULL) {
            uintptr_t address = (uintptr_t)symbol->value;
            const uint8_t *work = NULL;
            for (int i = 0; i < first.object->n_sections; ++i) {
                Section *section = &first.object->sections[i];
                if (section->start && address >= section->exec_start &&
                    address - section->exec_start <= section->size &&
                    sizeof(uint32_t) <= section->size - (address - section->exec_start)) {
                    work = (const uint8_t *)section->start + (address - section->exec_start);
                    if (noexec) CHECK((uintptr_t)work != address && section->alloc == SECTION_NOEXEC,
                                      "COMDAT did not use synthetic storage");
                    break;
                }
            }
            uint32_t value = 0;
            CHECK(work != NULL, "COMDAT symbol is outside its owner's sections");
            if (work) memcpy(&value, work, sizeof(value));
            CHECK(value == 29, "COMDAT selected unexpected contents");
        }
        setWeakSymbol(first.object, "ll_fixture_comdat");
        setImportSymbol(first.object, "ll_fixture_comdat");
        setImportSymbol(first.object, "ll_fixture_comdat");
        clearImportSymbol(first.object, "ll_fixture_comdat");
        CHECK(isSymbolWeak(first.object, "ll_fixture_comdat") &&
              !isSymbolImport(first.object, "ll_fixture_comdat"),
              "repeated symbol-info updates changed the flags");
        release_coff(&owner, &second);
        release_coff(&owner, &first);
        ll_mutex_unlock_for(&owner, &owner.linker_mutex);
        ll_exit_linker(&owner);
        CHECK(host_a.live == 0, "COMDAT ownership ledger is not empty");
    }
    if (failures) return 1;
    (void)puts("pei386_comdat: PASS");
    return 0;
}
