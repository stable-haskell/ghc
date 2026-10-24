#define _GNU_SOURCE 1
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/Elf.h"
#include "linker/MMap.h"
#include "linker/M32Alloc.h"

#if !defined(linux_HOST_OS) || !defined(x86_64_HOST_ARCH) \
    || !defined(OBJFORMAT_ELF)
#error "elf_section_errors requires the generated Linux x86_64 ELF target"
#endif

#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

enum { MAX_OWNED = 256, MAX_MAPS = 32, SECTION_COUNT = 4, BSS_BYTES = 32 };
typedef struct {
    void *owned[MAX_OWNED];
    size_t live;
    void *maps[MAX_MAPS];
    size_t extents[MAX_MAPS];
    size_t map_live;
    unsigned logs;
    char last_log[LL_ERROR_CAPACITY];
    bool active;
    bool fail_open;
    bool fail_anon;
    bool fail_file_map;
    bool fail_m32;
    ll_status m32_status;
    unsigned injections;
    unsigned opens;
    unsigned closes;
    int fd;
} Host;

static ll_linker owner;
static Host host;
static Host default_host;
static unsigned failures;

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "elf_section_errors:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static void remember(Host *h, void *p)
{
    if (p == NULL || h->live == MAX_OWNED) {
        (void)fprintf(stderr, "fixture allocator unavailable or ledger full\n");
        _Exit(98);
    }
    h->owned[h->live++] = p;
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    Host *h = ctx;
    (void)what;
    void *p = malloc(size == 0 ? 1 : size);
    remember(h, p);
    return p;
}

static void *host_calloc(void *ctx, size_t count, size_t size, const char *what)
{
    Host *h = ctx;
    (void)what;
    void *p = calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
    remember(h, p);
    return p;
}

static void *host_realloc(void *ctx, void *p, size_t size, const char *what)
{
    Host *h = ctx;
    if (p == NULL) return host_malloc(ctx, size, what);
    for (size_t i = 0; i < h->live; ++i) {
        if (h->owned[i] == p) {
            void *next = realloc(p, size == 0 ? 1 : size);
            if (next == NULL) _Exit(98);
            h->owned[i] = next;
            return next;
        }
    }
    (void)fprintf(stderr, "fixture realloc has wrong owner\n");
    _Exit(98);
}

static void host_free(void *ctx, void *p)
{
    Host *h = ctx;
    if (p == NULL) return;
    for (size_t i = 0; i < h->live; ++i) {
        if (h->owned[i] == p) {
            h->owned[i] = h->owned[--h->live];
            free(p);
            return;
        }
    }
    CHECK(false, "free has wrong owner");
}

static void host_log(void *ctx, int kind, const char *fmt, va_list ap)
{
    Host *h = ctx;
    (void)kind;
    int n = vsnprintf(h->last_log, sizeof(h->last_log), fmt, ap);
    CHECK(n >= 0 && (size_t)n < sizeof(h->last_log), "log was truncated");
    ++h->logs;
    errno = EIO;
}

static _Noreturn void host_fatal(void *ctx, const char *fmt, va_list ap)
{
    (void)ctx;
    (void)vfprintf(stderr, fmt, ap);
    _Exit(99);
}

static size_t page_size(void *ctx)
{
    (void)ctx;
    long n = sysconf(_SC_PAGESIZE);
    if (n <= 0) _Exit(98);
    return (size_t)n;
}

static ll_compat_host_ops host_ops(Host *h)
{
    return (ll_compat_host_ops){
        .ctx = h, .malloc_ = host_malloc, .calloc_ = host_calloc,
        .realloc_ = host_realloc, .free_ = host_free, .log = host_log,
        .fatal = host_fatal, .page_size = page_size
    };
}

static void track_map(ll_linker *L, void *p, size_t bytes)
{
    Host *h = L->services.host.ctx;
    size_t extent = ll_round_up_to_page_for(L, bytes);
    CHECK(p != NULL, "could not map positive fixture");
    if (p == NULL) _Exit(98);
    for (size_t i = 0; i < h->map_live; ++i) {
        if (h->maps[i] == p) {
            CHECK(h->extents[i] == extent, "nested mapping changed extent");
            return;
        }
    }
    if (h->map_live == MAX_MAPS) _Exit(98);
    h->maps[h->map_live] = p;
    h->extents[h->map_live++] = extent;
}

void *__real_ll_mmap_anon_for_linker(ll_linker *, size_t);
void *__real_ll_mmap_for_linker(ll_linker *, size_t, MemoryAccess,
                                uint32_t, int, int);
void __real_ll_munmap_for_linker(ll_linker *, void *, size_t, const char *);
void *__real_m32_alloc_for(ll_linker *, m32_allocator *, size_t, size_t);
int __real_open(const char *, int, ...);
int __real_close(int);

void *__wrap_ll_mmap_anon_for_linker(ll_linker *L, size_t bytes)
{
    Host *h = L->services.host.ctx;
    if (h->active && h->fail_anon) {
        ++h->injections;
        errno = ENOMEM;
        return NULL;
    }
    void *p = __real_ll_mmap_anon_for_linker(L, bytes);
    if (p != NULL) track_map(L, p, bytes);
    return p;
}

void *__wrap_ll_mmap_for_linker(ll_linker *L, size_t bytes,
                                MemoryAccess access, uint32_t flags,
                                int fd, int offset)
{
    Host *h = L->services.host.ctx;
    if (h->active && h->fail_file_map && fd >= 0) {
        CHECK(fd == h->fd && fcntl(fd, F_GETFD) >= 0,
              "file mapping failure did not use the reopened live fd");
        ++h->injections;
        errno = ENOMEM;
        return NULL;
    }
    void *p = __real_ll_mmap_for_linker(L, bytes, access, flags, fd, offset);
    if (p != NULL) track_map(L, p, bytes);
    return p;
}

void __wrap_ll_munmap_for_linker(ll_linker *L, void *p, size_t bytes,
                                 const char *caller)
{
    Host *h = L->services.host.ctx;
    bool found = false;
    for (size_t i = 0; i < h->map_live; ++i) {
        if (h->maps[i] == p) {
            CHECK(h->extents[i] == ll_round_up_to_page_for(L, bytes),
                  "cleanup used the wrong mapping extent");
            --h->map_live;
            h->maps[i] = h->maps[h->map_live];
            h->extents[i] = h->extents[h->map_live];
            found = true;
            break;
        }
    }
    CHECK(found, "cleanup released a mapping absent from the owner ledger");
    __real_ll_munmap_for_linker(L, p, bytes, caller);
}

void *__wrap_m32_alloc_for(ll_linker *L, m32_allocator *allocator,
                           size_t bytes, size_t alignment)
{
    Host *h = L->services.host.ctx;
    if (h->active && h->fail_m32) {
        ++h->injections;
        if (h->m32_status != LL_OK)
            ll_error_set(L, h->m32_status, "specific M32 failure");
        return NULL;
    }
    return __real_m32_alloc_for(L, allocator, bytes, alignment);
}

int __wrap_open(const char *path, int flags, ...)
{
    /* The backend's section reopen uses O_RDONLY without a creation mode. */
    if (flags != O_RDONLY) {
        (void)fprintf(stderr, "unexpected open flags in section unit\n");
        _Exit(98);
    }
    if (host.active) {
        ++host.opens;
        if (host.fail_open) {
            ++host.injections;
            errno = ENOENT;
            return -1;
        }
    }
    int fd = __real_open(path, flags);
    if (host.active) host.fd = fd;
    return fd;
}

int __wrap_close(int fd)
{
    if (host.active && fd == host.fd) ++host.closes;
    return __real_close(fd);
}

static unsigned char payload_byte(size_t i)
{
    return (unsigned char)((i * 37u + 19u) & 255u);
}

/* Ordinary relocatable storage, with no symbols, GOT, COMMON or relocations.
 * Both section orders exercise cleanup after an earlier completed section. */
static ObjectCode *make_object(const char *path, size_t data_size, bool bss_first)
{
    size_t page = page_size(NULL);
    size_t bytes = page + data_size;
    unsigned char *image = calloc(1, bytes);
    if (image == NULL) _Exit(98);
    Elf_Ehdr *header = (Elf_Ehdr *)image;
    memcpy(header->e_ident, ELFMAG, SELFMAG);
    header->e_ident[EI_CLASS] = ELFCLASS64;
    header->e_ident[EI_DATA] = ELFDATA2LSB;
    header->e_ident[EI_VERSION] = EV_CURRENT;
    header->e_type = ET_REL;
    header->e_machine = EM_X86_64;
    header->e_version = EV_CURRENT;
    header->e_ehsize = sizeof(*header);
    header->e_shoff = sizeof(*header);
    header->e_shentsize = sizeof(Elf_Shdr);
    header->e_shnum = SECTION_COUNT;
    header->e_shstrndx = 3;
    Elf_Shdr *sh = (Elf_Shdr *)(image + header->e_shoff);
    static const char names[] = "\0.data\0.bss\0.shstrtab\0";
    size_t names_offset = sizeof(*header) + SECTION_COUNT * sizeof(*sh);
    if (names_offset + sizeof(names) >= page) _Exit(98);
    memcpy(image + names_offset, names, sizeof(names));
    size_t data_index = bss_first ? 2 : 1;
    size_t bss_index = bss_first ? 1 : 2;
    sh[data_index] = (Elf_Shdr){
        .sh_name = 1, .sh_type = SHT_PROGBITS, .sh_flags = SHF_ALLOC | SHF_WRITE,
        .sh_offset = page, .sh_size = data_size, .sh_addralign = 8
    };
    sh[bss_index] = (Elf_Shdr){
        .sh_name = 7, .sh_type = SHT_NOBITS, .sh_flags = SHF_ALLOC | SHF_WRITE,
        .sh_size = BSS_BYTES, .sh_addralign = 8
    };
    sh[3] = (Elf_Shdr){
        .sh_name = 12, .sh_type = SHT_STRTAB, .sh_offset = names_offset,
        .sh_size = sizeof(names), .sh_addralign = 1
    };
    for (size_t i = 0; i < data_size; ++i) image[page + i] = payload_byte(i);
    int fd = __real_open(path, O_WRONLY | O_TRUNC);
    if (fd < 0) _Exit(98);
    size_t done = 0;
    while (done < bytes) {
        ssize_t n = write(fd, image + done, bytes - done);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) _Exit(98);
        done += (size_t)n;
    }
    CHECK(__real_close(fd) == 0, "fixture file close failed");
    fd = __real_open(path, O_RDONLY);
    if (fd < 0) _Exit(98);
    void *mapped = mmap(NULL, bytes, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
    CHECK(__real_close(fd) == 0, "fixture mapping fd close failed");
    free(image);
    if (mapped == MAP_FAILED) _Exit(98);
    track_map(&owner, mapped, bytes);
    ObjectCode *oc = mkOc(&owner, STATIC_OBJECT, (pathchar *)path,
                           mapped, (int)bytes, true, NULL, 0);
    CHECK(ll_init_object(oc) == 1, "ordinary storage fixture failed ELF validation");
    return oc;
}

enum Case { POSITIVE, REOPEN, BSS, FILE_MAP, M32_FALLBACK, M32_SPECIFIC };

static void run_case(const char *path, enum Case which)
{
    size_t before_live = host.live;
    size_t before_maps = host.map_live;
    size_t data_size = which == M32_FALLBACK || which == M32_SPECIFIC
                       ? 32 : page_size(NULL);
    bool bss_first = which != BSS;
    ll_error_clear(&owner);
    ObjectCode *oc = make_object(path, data_size, bss_first);
    host.fail_open = which == REOPEN;
    host.fail_anon = which == BSS;
    host.fail_file_map = which == FILE_MAP;
    host.fail_m32 = which == M32_FALLBACK || which == M32_SPECIFIC;
    host.m32_status = which == M32_SPECIFIC ? LL_E_STATE : LL_OK;
    host.opens = host.closes = host.injections = 0;
    host.fd = -1;
    host.active = true;
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    int result = ocGetNames_ELF(oc);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    CHECK(host.opens == 1, "section pass did not attempt one backing-file reopen");
    CHECK(host.closes == (which == REOPEN ? 0u : 1u),
          "section pass did not close its reopened descriptor exactly once");
    if (which != REOPEN) {
        errno = 0;
        CHECK(fcntl(host.fd, F_GETFD) == -1 && errno == EBADF,
              "reopened descriptor remained live after section pass");
    }
    ll_status expected = which == POSITIVE ? LL_OK
                         : which == REOPEN || which == FILE_MAP ? LL_E_IO
                         : which == M32_SPECIFIC ? LL_E_STATE : LL_E_NOMEM;
    CHECK(result == (which == POSITIVE ? 1 : 0), "section pass returned wrong result");
    CHECK(owner.error_status == expected, "section pass returned wrong owner status");
    CHECK(host.injections == (which == POSITIVE ? 0u : 1u),
          "requested failure injection was not reached exactly once");
    if (which == POSITIVE) {
        Section *data = &oc->sections[2];
        Section *bss = &oc->sections[1];
        CHECK(data->alloc == SECTION_MMAP && data->size == data_size,
              "ordinary file-backed section took the wrong storage path");
        CHECK(bss->alloc == SECTION_MMAP && bss->size == BSS_BYTES,
              "BSS section took the wrong storage path");
        for (size_t i = 0; i < data_size; ++i)
            CHECK(((unsigned char *)data->start)[i] == payload_byte(i),
                  "ordinary section payload changed");
        for (size_t i = 0; i < BSS_BYTES; ++i)
            CHECK(((unsigned char *)bss->start)[i] == 0, "BSS was not zeroed");
    } else {
        CHECK(owner.error_message[0] != '\0', "failure omitted its diagnostic");
        if (which == M32_SPECIFIC)
            CHECK(strcmp(owner.error_message, "specific M32 failure") == 0,
                  "section fallback overwrote the specific M32 error");
        if (which == BSS) {
            char expected_message[128];
            int n = snprintf(expected_message, sizeof(expected_message),
                             "failed to mmap memory for bss. errno = %d", ENOMEM);
            CHECK(n > 0 && (size_t)n < sizeof(expected_message),
                  "expected BSS diagnostic formatting failed");
            CHECK(strcmp(owner.error_message, expected_message) == 0,
                  "BSS status lost the allocation errno");
            CHECK(strcmp(host.last_log, expected_message) == 0,
                  "BSS log lost the allocation errno");
        }
        if (which != REOPEN)
            CHECK(oc->sections[1].start != NULL,
                  "failure did not follow a completed earlier section");
    }
    host.active = false;
    ll_status status = owner.error_status;
    char message[LL_ERROR_CAPACITY];
    memcpy(message, owner.error_message, sizeof(message));
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    ll_discard_object(oc);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    CHECK(host.live == before_live, "normal discard leaked owner allocations");
    CHECK(host.map_live == before_maps, "normal discard leaked section/image maps");
    CHECK(owner.error_status == status
              && memcmp(message, owner.error_message, sizeof(message)) == 0,
          "cleanup changed the primary error");
    CHECK(owner.gc.objects == NULL && owner.gc.loaded_objects == NULL,
          "unregistered section unit changed owner object lists");
    CHECK(ll_default_linker.error_status == LL_E_STATE
              && strcmp(ll_default_linker.error_message, "default sentinel") == 0
              && default_host.logs == 0 && default_host.live == 0,
          "explicit owner operation touched the default owner");
}

int main(void)
{
    ll_compat_config config = {0};
    ll_compat_host_ops ops = host_ops(&default_host);
    ll_install_host(&ops, &config);
    ll_error_set(&ll_default_linker, LL_E_STATE, "default sentinel");
    ops = host_ops(&host);
    ll_install_host_for(&owner, &ops, &config);
    ll_mutex_init_for(&owner.services, &owner.linker_mutex);
    owner.symhash = allocStrHashTable(&owner);
    ll_init_linker_mmap(&owner);
    char path[] = "/tmp/liblink-elf-section-XXXXXX";
    int fd = mkstemp(path);
    if (fd < 0) return 1;
    CHECK(__real_close(fd) == 0, "temporary fixture close failed");
    run_case(path, POSITIVE);
    for (enum Case which = REOPEN; which <= M32_SPECIFIC; ++which) {
        run_case(path, which);
        run_case(path, POSITIVE);
    }
    CHECK(unlink(path) == 0, "temporary fixture cleanup failed");
    freeStrHashTable(owner.symhash, NULL);
    owner.symhash = NULL;
    ll_mutex_close_for(&owner.services, &owner.linker_mutex);
    CHECK(host.live == 0 && host.map_live == 0, "unit left owned resources live");
    if (failures != 0) return 1;
    (void)puts("ELF section errors and cleanup: PASS");
    return 0;
}
