/* Reuse archive construction and owner ledgers, with a real native object. */
#define main archive_status_original_main
#include "archive_error_status.c"
#undef main
#include "ll_format.h"
#include "Linker.c"

enum { NATIVE_IMAGE_CAPACITY = 32768, NATIVE_ARCHIVE_CAPACITY = 65536 };

static void dispose_loaded(ll_linker *owner)
{
    while (owner->gc.loaded_objects != NULL) {
        ObjectCode *object = owner->gc.loaded_objects;
        owner->gc.loaded_objects = object->next_loaded_object;
        /* This fixture's registration hook records calls without inserting a
         * second list. Remove published symbols before releasing their names. */
        removeOcSymbols(object);
        freeObjectCode(object);
    }
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        (void)fprintf(stderr, "usage: format_archive native-object\n");
        return 2;
    }
    unsigned char image[NATIVE_IMAGE_CAPACITY];
    FILE *input = fopen(argv[1], "rb");
    CHECK(input != NULL, "could not open compiler object");
    if (input == NULL) return 1;
    size_t size = fread(image, 1, sizeof(image), input);
    CHECK(!ferror(input) && feof(input) && size > 8,
          "compiler object is empty, too large, or unreadable");
    CHECK(fclose(input) == 0, "could not close compiler object");
    if (failures != 0) return 1;
    char size_text[32];
    int count = snprintf(size_text, sizeof(size_text), "%zu", size);
    CHECK(count > 0 && (size_t)count < sizeof(size_text), "object size overflow");

    ll_linker owner = {0};
    ll_compat_host_ops ops = make_ops(&host_a);
    ll_compat_config config = {.always_pic = true};
    ll_install_host_for(&owner, &ops, &config);
    ll_init_linker(&owner, 0);
    ll_init_linker_mmap(&owner);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    CHECK(loadObj_(&owner, (pathchar *)argv[1]), "standalone compiler object failed");
    ObjectCode *standalone = owner.gc.loaded_objects;
    const struct ll_format_ops *selected = standalone != NULL ? standalone->format_ops : NULL;
    CHECK(selected != NULL && selected == ll_format_native(),
          "standalone object did not use its backend-owned table");
    dispose_loaded(&owner);

    char dir_template[] = "/tmp/liblink-format-archive-XXXXXX";
    char *dir = mkdtemp(dir_template);
    CHECK(dir != NULL, "could not create fixture directory");
    if (dir != NULL) {
        for (unsigned which = 0; which < 3; ++which) {
            char bytes[NATIVE_ARCHIVE_CAPACITY];
            char path[256];
            const char *leaf = which == 0 ? "ordinary.a" :
                               which == 1 ? "skip.a" : "later-error.a";
            make_path(path, sizeof(path), dir, leaf);
            size_t used = make_archive(bytes, sizeof(bytes));
            if (which == 1) {
                used = append_member(bytes, sizeof(bytes), used, "/", "0", NULL, 0, false);
                used = append_member(bytes, sizeof(bytes), used, "payload/", "5", "hello", 5, true);
            }
            used = append_member(bytes, sizeof(bytes), used, "member.data/", size_text,
                                 image, size, true);
            if (which == 2)
                used = append_member(bytes, sizeof(bytes), used, "short.o/", "8", image, 8, false);
            write_fixture(path, bytes, used);
            unsigned registered_before = host_a.registrations;
            ll_error_clear(&owner);
            ll_hs_int result = loadArchive_(&owner, (pathchar *)path);
            CHECK(which == 2 ? result == 0 && owner.error_status == LL_E_FORMAT :
                               result != 0 && owner.error_status == LL_OK,
                  "archive result or primary status differs");
            CHECK(which != 2 || owner.error_message[0] != '\0',
                  "later member lost its format diagnostic during cleanup");
            ObjectCode *member = owner.gc.loaded_objects;
            CHECK(host_a.registrations == registered_before + 1,
                  "skipped or failed member was registered");
            CHECK(member != NULL && member->next_loaded_object == NULL &&
                  member->format_ops == selected && member->archiveMemberName != NULL,
                  "successful member lost its standalone format binding or ownership");
            CHECK(member != NULL && member->status == OBJECT_LOADED && !member->initializersRan,
                  "archive member resolved or initialized eagerly");
            if (member != NULL) {
                CHECK(ocTryLoad(member) && member->status == OBJECT_LOADED,
                      "unused archive member did not retain lazy behavior");
                CHECK(owner.error_status == LL_OK && owner.error_message[0] == '\0',
                      "fresh unused-member operation retained an earlier diagnostic");
            }
            dispose_loaded(&owner);
            CHECK(unlink(path) == 0, "could not remove archive fixture");
        }
        CHECK(rmdir(dir) == 0, "could not remove fixture directory");
    }
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    CHECK(host_a.live == 0 && host_a.alloc_calls == host_a.free_calls,
          "native archive cases leaked owner storage");
    CHECK(host_a.fatal_calls == 0, "native archive cases used fatal callback");
    if (failures != 0) return 1;
    (void)puts("format_archive: PASS (backend binding, admission, lazy members, partial retention)");
    return 0;
}
