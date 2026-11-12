/* Real host operations; no loaded object code or protected writes execute. */
#define _DARWIN_C_SOURCE 1
#define _DEFAULT_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/BackendMemory.h"
#include "linker/MMap.h"
#include "ll_fs.h"
#include "ll_path.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(mingw32_HOST_OS)
#include <windows.h>
#elif defined(darwin_HOST_OS)
#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <unistd.h>
#else
#include <unistd.h>
#endif

static unsigned failures;
static unsigned memory_failures;
#define CHECK(condition, message) do { if (!(condition)) { \
    (void)fprintf(stderr, "backend_memory:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

static size_t page_size(void *ctx)
{
    (void)ctx;
#if defined(mingw32_HOST_OS)
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    return info.dwPageSize;
#else
    long bytes = sysconf(_SC_PAGESIZE);
    return bytes > 0 ? (size_t)bytes : 4096;
#endif
}

static void log_message(void *ctx, int kind, const char *fmt, va_list ap)
{
    (void)ctx;
    (void)kind;
    (void)fmt;
    (void)ap;
}

static void memory_failure(void *ctx)
{
    (void)ctx;
    ++memory_failures;
}

static bool mapping_is_unmapped(void *memory, size_t bytes)
{
#if defined(mingw32_HOST_OS)
    (void)bytes;
    MEMORY_BASIC_INFORMATION info;
    const SIZE_T count = VirtualQuery(memory, &info, sizeof(info));
    CHECK(count != 0, "cannot query host reservation");
    return count != 0 && info.State == MEM_FREE;
#elif defined(darwin_HOST_OS)
    (void)bytes;
    /* mincore need not report a hole on Darwin. Query the next VM region
     * instead; its start must move beyond an address after it is unmapped. */
    mach_vm_address_t address = (mach_vm_address_t)(uintptr_t)memory;
    mach_vm_size_t size = 0;
    vm_region_basic_info_data_64_t info;
    mach_msg_type_number_t count = VM_REGION_BASIC_INFO_COUNT_64;
    mach_port_t object = MACH_PORT_NULL;
    const kern_return_t result = mach_vm_region(mach_task_self(), &address,
        &size, VM_REGION_BASIC_INFO_64, (vm_region_info_t)&info, &count, &object);
    if (object != MACH_PORT_NULL)
        (void)mach_port_deallocate(mach_task_self(), object);
    CHECK(result == KERN_SUCCESS || result == KERN_INVALID_ADDRESS,
          "cannot query host mapping");
    return result == KERN_INVALID_ADDRESS ||
        (result == KERN_SUCCESS && address > (mach_vm_address_t)(uintptr_t)memory);
#else
    unsigned char resident;
    errno = 0;
    const int result = mincore(memory, bytes, &resident);
    CHECK(result == 0 || (result == -1 && errno == ENOMEM),
          "cannot query host mapping");
    return result == -1 && errno == ENOMEM;
#endif
}

int main(void)
{
    static const unsigned char contents[] = {0, 1, 2, 3, 0xfe, 0xff, 7, 8};
#if defined(mingw32_HOST_OS)
    wchar_t directory[512];
    wchar_t path[512];
    DWORD count = GetTempPathW(512, directory);
    CHECK(count > 0 && count < 512, "temporary path is unavailable or too long");
    if (failures != 0) return 1;
    /* The real wide-path service must retain a non-ASCII filename. */
    CHECK(GetTempFileNameW(directory, L"ll\u03bb", 0, path) != 0,
          "cannot create wide-path fixture");
    if (failures != 0) return 1;
    FILE *stream = FS(fwopen)(path, L"wb");
#else
    char path[] = "/tmp/liblink-backend-memory-XXXXXX";
    int initial_fd = mkstemp(path);
    CHECK(initial_fd >= 0, "cannot create file fixture");
    if (initial_fd < 0) return 1;
    FILE *stream = fdopen(initial_fd, "wb");
#endif
    CHECK(stream != NULL, "cannot write file fixture");
    if (stream != NULL) {
        CHECK(fwrite(contents, 1, sizeof(contents), stream) == sizeof(contents),
              "short fixture write");
        CHECK(fclose(stream) == 0, "fixture close failed");
    }
#if !defined(mingw32_HOST_OS)
    else ll_backend_close_file(initial_fd);
#endif
    if (failures != 0) {
#if defined(mingw32_HOST_OS)
        (void)DeleteFileW(path);
#else
        (void)unlink(path);
#endif
        return 1;
    }

    ll_linker owner = {0};
    ll_compat_host_ops host = {.page_size = page_size, .log = log_message,
                               .on_mem_failure = memory_failure};
    ll_compat_config config = {.always_pic = true};
    ll_install_host_for(&owner, &host, &config);
    ll_init_linker_mmap(&owner);
    ll_error_set(&owner, LL_E_UNRESOLVED, "host-memory primary sentinel");
    const size_t page = page_size(NULL);
    void *memory = ll_backend_map_anon_write(&owner, page);
    CHECK(memory != NULL, "anonymous host allocation failed");
    if (memory != NULL) {
        memcpy(memory, contents, sizeof(contents));
        CHECK(ll_backend_protect_readonly(memory, page), "read-only protection failed");
        CHECK(memcmp(memory, contents, sizeof(contents)) == 0,
              "protection changed anonymous contents");
        ll_munmap_for_linker(&owner, memory, page, "backend_memory anonymous");
    }
    /* Use the actual object constructor and physical disposal path too. A
     * direct unmap above cannot detect a missing SECTION_MMAP teardown case. */
    memory = ll_backend_map_anon_write(&owner, page);
    CHECK(memory != NULL, "object-owned host allocation failed");
    if (memory != NULL) {
        ObjectCode *object = mkOc(&owner, STATIC_OBJECT, WSTR("host-memory"),
                                  NULL, 0, false, NULL, 0);
        object->sections = LL_CALLOC_FOR(&owner, 1, sizeof(Section),
                                         "backend_memory section");
        CHECK(object->sections != NULL, "object section allocation failed");
        if (object->sections != NULL) {
            object->n_sections = 1;
            object->sections[0].start = memory;
            object->sections[0].size = page;
            object->sections[0].alloc = SECTION_MMAP;
            object->sections[0].mapped_start = memory;
            object->sections[0].mapped_size = page;
            CHECK(!mapping_is_unmapped(memory, page),
                  "object mapping is absent before disposal");
            freeObjectCode(object);
            CHECK(mapping_is_unmapped(memory, page),
                  "object disposal retained its mapped section");
        } else {
            freeObjectCode(object);
            ll_munmap_for_linker(&owner, memory, page, "backend_memory partial");
        }
    }
    CHECK(!ll_backend_protect_readonly(NULL, page),
          "unmapped address unexpectedly accepted protection");

    int fd = ll_backend_open_readonly(path);
    CHECK(fd >= 0, "host read-only open failed");
    if (fd >= 0) {
        ll_status failure_status = LL_OK;
        memory = ll_backend_map_file_write(&owner, fd, page, 0, &failure_status);
#if defined(mingw32_HOST_OS) || !RTS_LINKER_USE_MMAP
        CHECK(memory == NULL && failure_status == LL_E_UNSUPPORTED,
              "unavailable file mapping did not report its capability");
#else
        CHECK(memory != NULL, "file-backed host mapping failed");
        if (memory != NULL) {
            CHECK(memcmp(memory, contents, sizeof(contents)) == 0,
                  "file mapping changed binary contents");
            CHECK(ll_backend_protect_readonly(memory, page),
                  "file mapping protection failed");
            ll_munmap_for_linker(&owner, memory, page, "backend_memory file");
        }
        memory = ll_backend_map_file_write(&owner, -1, page, 0, &failure_status);
        CHECK(memory == NULL && failure_status == LL_E_IO,
              "file mapping failure lost its native status");
#endif
        ll_backend_close_file(fd);
    }
#if defined(mingw32_HOST_OS)
    CHECK(DeleteFileW(path), "cannot remove wide-path fixture");
#else
    CHECK(unlink(path) == 0, "cannot remove file fixture");
#endif
    CHECK(ll_backend_open_readonly(path) < 0, "missing file unexpectedly opened");
#if defined(mingw32_HOST_OS) || !RTS_LINKER_USE_MMAP
    CHECK(memory_failures == 0, "unavailable service reported memory exhaustion");
#else
    CHECK(memory_failures == 1, "failed mapping did not report memory failure once");
#endif
    CHECK(owner.error_status == LL_E_UNRESOLVED &&
          strcmp(owner.error_message, "host-memory primary sentinel") == 0,
          "host operations changed the caller's error policy");
    if (failures != 0) return 1;
    (void)puts("backend_memory: PASS (host mapping, protection, paths, failure policy)");
    return 0;
}
