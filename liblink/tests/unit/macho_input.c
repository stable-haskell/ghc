#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/MachO.h"
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#if !defined(OBJFORMAT_MACHO) || !(defined(x86_64_HOST_ARCH) || defined(aarch64_HOST_ARCH) || defined(arm64_HOST_ARCH))
#error "macho_input is only built for a real supported native Mach-O target"
#endif

enum { MAX_OWNED = 1024 };
typedef struct {
    void *owned[MAX_OWNED];
    const char *what[MAX_OWNED];
    size_t live;
    unsigned allocs;
    unsigned frees;
    unsigned logs;
    unsigned fatals;
} Host;

typedef struct {
    unsigned allocs;
    unsigned frees;
    size_t live;
    size_t hash_lists;
    StrHashTable *symhash;
    ObjectCode *objects;
    ObjectCode *loaded;
    uint64_t unloaded;
} Snapshot;

static Host owner_host;
static Host default_host;
static ll_linker owner;
static unsigned failures;
static char default_sentinel[] = "macho-input-default-owner-sentinel";

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "macho_input:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static void remember(Host *host, void *ptr, const char *what)
{
    CHECK(ptr != NULL, "host allocation returned NULL");
    CHECK(host->live < MAX_OWNED, "host allocation ledger overflow");
    if (ptr != NULL && host->live < MAX_OWNED) {
        host->owned[host->live] = ptr;
        host->what[host->live++] = what;
    }
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    void *ptr = malloc(size == 0 ? 1 : size);
    ++host->allocs;
    remember(host, ptr, what);
    return ptr;
}

static void *host_calloc(void *ctx, size_t count, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    void *ptr = calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
    ++host->allocs;
    remember(host, ptr, what);
    return ptr;
}

static void *host_realloc(void *ctx, void *ptr, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    if (ptr == NULL) return host_malloc(ctx, size, what);
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == ptr) {
            void *next = realloc(ptr, size == 0 ? 1 : size);
            if (next != NULL) host->owned[i] = next;
            return next;
        }
    }
    CHECK(false, "realloc pointer was not owned by this linker");
    return NULL;
}

static void host_free(void *ctx, void *ptr)
{
    Host *host = ctx;
    if (ptr == NULL) return;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == ptr) {
            host->owned[i] = host->owned[--host->live];
            host->what[i] = host->what[host->live];
            ++host->frees;
            free(ptr);
            return;
        }
    }
    CHECK(false, "free pointer was not owned by this linker");
}

static void host_log(void *ctx, int kind, const char *fmt, va_list ap)
{
    Host *host = ctx;
    char message[1024];
    int n = vsnprintf(message, sizeof(message), fmt, ap);
    CHECK(n >= 0 && (size_t)n < sizeof(message), "log message truncated");
    (void)kind;
    ++host->logs;
}

static _Noreturn void host_fatal(void *ctx, const char *fmt, va_list ap)
{
    Host *host = ctx;
    char message[1024];
    (void)vsnprintf(message, sizeof(message), fmt, ap);
    ++host->fatals;
    (void)fprintf(stderr, "unexpected Mach-O fatal: %s\n", message);
    _Exit(98);
}

static size_t host_page_size(void *ctx)
{
    (void)ctx;
    long page_size = sysconf(_SC_PAGESIZE);
    return page_size > 0 ? (size_t)page_size : 4096u;
}

static ll_compat_host_ops make_ops(Host *host)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = host;
    ops.malloc_ = host_malloc;
    ops.calloc_ = host_calloc;
    ops.realloc_ = host_realloc;
    ops.free_ = host_free;
    ops.log = host_log;
    ops.fatal = host_fatal;
    ops.page_size = host_page_size;
    return ops;
}

static Snapshot snapshot(void)
{
    size_t hash_lists=0;
    for (size_t i=0;i<owner_host.live;i++)
        if (strcmp(owner_host.what[i],"allocHashList")==0) ++hash_lists;
    return (Snapshot){owner_host.allocs, owner_host.frees, owner_host.live, hash_lists,
                      owner.symhash, owner.gc.objects, owner.gc.loaded_objects,
                      (uint64_t)owner.gc.n_unloaded_objects};
}

static void check_delta(Snapshot before, const char *message)
{
    Snapshot after = snapshot();
    CHECK(after.live == before.live, message);
    CHECK(after.allocs - before.allocs == after.frees - before.frees, message);
    CHECK(after.symhash == before.symhash && after.objects == before.objects
              && after.loaded == before.loaded && after.unloaded == before.unloaded,
          message);
}

static void check_loaded_delta(Snapshot before, const char *message)
{
    Snapshot after=snapshot();
    size_t retained_hash_lists=after.hash_lists-before.hash_lists;
    CHECK(after.live-before.live==retained_hash_lists,message);
    CHECK(after.allocs-before.allocs==after.frees-before.frees+retained_hash_lists,
          message);
    CHECK(after.symhash==before.symhash && after.objects==before.objects
              && after.loaded==before.loaded && after.unloaded==before.unloaded,
          message);
}

static unsigned char *read_image(const char *path, size_t *size)
{
    FILE *file = fopen(path, "rb");
    if (file == NULL) return NULL;
    if (fseek(file, 0, SEEK_END) != 0) { (void)fclose(file); return NULL; }
    long end = ftell(file);
    if (end <= 0 || end > INT_MAX || fseek(file, 0, SEEK_SET) != 0) {
        (void)fclose(file);
        return NULL;
    }
    unsigned char *bytes = malloc((size_t)end);
    if (bytes == NULL) { (void)fclose(file); return NULL; }
    bool ok = fread(bytes, 1, (size_t)end, file) == (size_t)end;
    if (fclose(file) != 0) ok = false;
    if (!ok) { free(bytes); return NULL; }
    *size = (size_t)end;
    return bytes;
}

static bool image_commands(unsigned char *bytes, size_t size,
                           MachOHeader **header_out,
                           MachOSymtabCommand **sym_out,
                           MachOSegmentCommand **seg_out)
{
    if (size < sizeof(MachOHeader)) return false;
    MachOHeader *header = (MachOHeader *)bytes;
    size_t pos = sizeof(*header);
    MachOSymtabCommand *sym = NULL;
    MachOSegmentCommand *seg = NULL;
    for (uint32_t i = 0; i < header->ncmds; ++i) {
        if (pos > size || size - pos < sizeof(MachOLoadCommand)) return false;
        MachOLoadCommand *cmd = (MachOLoadCommand *)(bytes + pos);
        if (cmd->cmdsize < sizeof(*cmd) || cmd->cmdsize > size - pos) return false;
        if (cmd->cmd == LC_SYMTAB && cmd->cmdsize >= sizeof(MachOSymtabCommand))
            sym = (MachOSymtabCommand *)cmd;
        if (cmd->cmd == LC_SEGMENT_64 && cmd->cmdsize >= sizeof(MachOSegmentCommand))
            seg = (MachOSegmentCommand *)cmd;
        pos += cmd->cmdsize;
    }
    *header_out = header;
    *sym_out = sym;
    *seg_out = seg;
    return sym != NULL && seg != NULL;
}

static MachOSection *section_at(unsigned char *bytes, size_t size,
                                uint32_t wanted_type, bool require_reloc,
                                bool require_payload)
{
    MachOHeader *header;
    MachOSymtabCommand *sym;
    MachOSegmentCommand *seg;
    if (!image_commands(bytes, size, &header, &sym, &seg)) return NULL;
    (void)header;
    (void)sym;
    MachOSection *sections = (MachOSection *)(seg + 1);
    if ((size_t)seg->nsects > (size - ((unsigned char *)sections - bytes)) / sizeof(*sections))
        return NULL;
    for (uint32_t i = 0; i < seg->nsects; ++i) {
        MachOSection *section = &sections[i];
        if ((wanted_type != UINT32_MAX && (section->flags & SECTION_TYPE) != wanted_type)
            || (require_reloc && section->nreloc == 0)
            || (require_payload && section->size == 0)) continue;
        return section;
    }
    return NULL;
}

typedef bool (*Mutation)(unsigned char *bytes, size_t size);
static bool bad_magic(unsigned char *b, size_t n) { (void)n; b[0] ^= 0x1u; return true; }
static bool bad_cmdsize(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g) || h->ncmds == 0) return false;
    MachOLoadCommand *c=(MachOLoadCommand *)(b+sizeof(*h)); c->cmdsize=0; return true;
}
static bool huge_cmdsize(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g) || h->ncmds == 0) return false;
    MachOLoadCommand *c=(MachOLoadCommand *)(b+sizeof(*h)); c->cmdsize=UINT32_MAX; return true;
}
static bool huge_section_count(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g)) return false;
    g->nsects=UINT32_MAX; return true;
}
static bool huge_command_count(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g)) return false;
    h->ncmds=UINT32_MAX; return true;
}
static bool bad_command_bytes(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g)) return false;
    h->sizeofcmds=UINT32_MAX; return true;
}
static bool bad_symoff(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g)) return false;
    s->symoff=(uint32_t)(n-1); return true;
}
static bool huge_symbol_count(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g)) return false;
    s->nsyms=UINT32_MAX; return true;
}
static bool bad_stroff(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g)) return false;
    s->stroff=(uint32_t)(n-1); s->strsize=UINT32_MAX; return true;
}
static bool bad_string_index(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g) || s->nsyms == 0) return false;
    MachONList *symbols=(MachONList *)(b+s->symoff);
    symbols[0].n_un.n_strx=UINT32_MAX; return true;
}
static bool unterminated_symbol_name(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g) || s->nsyms==0
        || s->stroff>n || s->strsize>n-s->stroff) return false;
    MachONList *symbols=(MachONList *)(b+s->symoff);
    uint32_t index=0;
    for (uint32_t i=0;i<s->nsyms;i++) {
        if (symbols[i].n_un.n_strx!=0 && symbols[i].n_un.n_strx<s->strsize) {
            index=symbols[i].n_un.n_strx; break;
        }
    }
    if (index==0) return false;
    memset(b+s->stroff+index,'X',s->strsize-index);
    return true;
}
static bool bad_symbol_section(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g) || s->nsyms == 0) return false;
    MachONList *symbols=(MachONList *)(b+s->symoff);
    for (uint32_t i=0;i<s->nsyms;i++) {
        if ((symbols[i].n_type & N_TYPE) == N_SECT) {
            symbols[i].n_sect=(uint8_t)(g->nsects+1); return true;
        }
    }
    return false;
}
static bool bad_payload(unsigned char *b, size_t n)
{
    MachOSection *section=section_at(b,n,UINT32_MAX,false,true);
    if (section==NULL) return false;
    if ((section->flags & SECTION_TYPE)==S_ZEROFILL
        || (section->flags & SECTION_TYPE)==S_GB_ZEROFILL) return false;
    section->offset=UINT32_MAX; return true;
}
static bool bad_alignment(unsigned char *b, size_t n)
{
    MachOSection *section=section_at(b,n,UINT32_MAX,false,true);
    if (section==NULL) return false;
    section->align=(uint32_t)(sizeof(size_t)*CHAR_BIT); return true;
}
static bool bad_reloc_span(unsigned char *b, size_t n)
{
    MachOSection *section=section_at(b,n,UINT32_MAX,true,false);
    if (section==NULL) return false;
    section->reloff=UINT32_MAX; return true;
}
static bool huge_reloc_count(unsigned char *b, size_t n)
{
    MachOSection *section=section_at(b,n,UINT32_MAX,true,false);
    if (section==NULL) return false;
    section->nreloc=UINT32_MAX; return true;
}
static bool bad_reloc_symbol(unsigned char *b, size_t n)
{
    MachOSection *section=section_at(b,n,UINT32_MAX,true,false);
    if (section==NULL) return false;
    if ((size_t)section->reloff > n
        || n-(size_t)section->reloff < sizeof(MachORelocationInfo)) return false;
    MachORelocationInfo *rel=(MachORelocationInfo *)(b+section->reloff);
    rel->r_extern=1; rel->r_symbolnum=0xffffffu; return true;
}
static bool bad_reloc_operand(unsigned char *b, size_t n)
{
    MachOSection *section=section_at(b,n,UINT32_MAX,true,false);
    if (section==NULL) return false;
    if ((size_t)section->reloff > n
        || n-(size_t)section->reloff < sizeof(MachORelocationInfo)) return false;
    MachORelocationInfo *rel=(MachORelocationInfo *)(b+section->reloff);
    rel->r_address=(int32_t)section->size; rel->r_length=3; return true;
}
static bool bad_init_shape(unsigned char *b, size_t n)
{
    MachOSection *section=section_at(b,n,S_MOD_INIT_FUNC_POINTERS,false,true);
    if (section==NULL || section->size<sizeof(void *)) return false;
    --section->size; return true;
}
static bool common_overflow(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g) || s->nsyms<2) return false;
    MachONList *symbols=(MachONList *)(b+s->symoff);
    unsigned found=0;
    for (uint32_t i=0;i<s->nsyms && found<2;i++) {
        if ((symbols[i].n_type & N_TYPE)==N_UNDF && symbols[i].n_value!=0) {
            symbols[i].n_value=UINT64_MAX; ++found;
        }
    }
    return found==2;
}
static bool common_sum_overflow(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g) || s->nsyms<2) return false;
    MachONList *symbols=(MachONList *)(b+s->symoff);
    uint64_t each=(uint64_t)PTRDIFF_MAX/2u+1u;
    unsigned found=0;
    for (uint32_t i=0;i<s->nsyms && found<2;i++) {
        if ((symbols[i].n_type & N_TYPE)==N_UNDF && symbols[i].n_value!=0) {
            symbols[i].n_value=each; ++found;
        }
    }
    return found==2;
}
static bool unterminated_fixed_names(unsigned char *b, size_t n)
{
    MachOHeader *h; MachOSymtabCommand *s; MachOSegmentCommand *g;
    if (!image_commands(b,n,&h,&s,&g)) return false;
    MachOSection *sections=(MachOSection *)(g+1);
    for (uint32_t i=0;i<g->nsects;i++) {
        memset(sections[i].sectname,'Q',sizeof(sections[i].sectname));
        memset(sections[i].segname,'R',sizeof(sections[i].segname));
    }
    return true;
}

static void run_case_checked(const char *label, const unsigned char *original,
                             size_t size, Mutation mutation, bool expected_valid,
                             bool load_symbols, bool check_aligned_zero)
{
    CHECK(size <= INT_MAX, "fixture exceeds ObjectCode fileSize");
    if (size > INT_MAX) return;
    Snapshot before=snapshot();
    bool mapped=expected_valid && load_symbols;
    unsigned char *image=mapped
        ? mmap(NULL,size,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANON,-1,0)
        : host_malloc(&owner_host,size,"macho fixture image");
    if (image==MAP_FAILED) image=NULL;
    CHECK(image!=NULL,"could not allocate fixture image");
    if (image==NULL) return;
    memcpy(image,original,size);
    if (mutation!=NULL) CHECK(mutation(image,size), "mutation precondition missing in compiled fixture");

    ObjectCode *oc=mkOc(&owner,STATIC_OBJECT,(pathchar *)label,(char *)image,
                        (int)size,mapped,NULL,0);
    CHECK(oc!=NULL,"mkOc returned NULL");
    if (oc==NULL) {
        if (mapped) (void)munmap(image,size);
        else host_free(&owner_host,image);
        return;
    }
    ll_error_clear(&owner);
    if (expected_valid) {
        int initialized=ll_init_object(oc);
        CHECK(initialized!=0 && owner.error_status==LL_OK,label);
        if (initialized && load_symbols) {
            int loaded=loadOc(oc);
            CHECK(loaded!=0 && owner.error_status==LL_OK,label);
            if (loaded && check_aligned_zero) {
                RtsSymbolInfo *info = lookupStrHashTable(
                    owner.symhash, "_wp24_macho_aligned_zero");
                CHECK(info != NULL, "aligned zero-fill symbol is missing from owner hash");
                if (info != NULL) {
                    CHECK(info->owner == oc,
                          "aligned zero-fill symbol was registered to the wrong object");
                    CHECK(info->value != NULL,
                          "aligned zero-fill symbol has no loaded address");
                    if (info->value != NULL) {
                        CHECK((uintptr_t)info->value % (uintptr_t)32768 == 0,
                              "loaded zero-fill symbol lost its 32 KiB alignment");
                        CHECK(*(int *)info->value == 0,
                              "loaded zero-fill symbol did not remain zero-initialized");
                    }
                }
            }
        }
    } else {
        int initialized=ll_init_object(oc);
        CHECK(initialized==0 && owner.error_status==LL_E_FORMAT,label);
        CHECK(oc->info==NULL,"invalid image reached ocInit_MachO before rejection");
    }
    CHECK(owner_host.fatals==0,"malformed image reached fatal callback");
    ll_discard_object(oc);
    if (expected_valid && load_symbols) check_loaded_delta(before,label);
    else check_delta(before,label);
}

static void run_case(const char *label, const unsigned char *original, size_t size,
                     Mutation mutation, bool expected_valid, bool load_symbols)
{
    run_case_checked(label, original, size, mutation, expected_valid, load_symbols,
                     false);
}

static void run_empty_image(void)
{
    Snapshot before=snapshot();
    unsigned char *empty=host_malloc(&owner_host,sizeof(uint32_t),"truncated image");
    if (empty==NULL) return;
    memset(empty,0,sizeof(uint32_t));
    ObjectCode *oc=mkOc(&owner,STATIC_OBJECT,(pathchar *)"truncated-header",
                        (char *)empty,(int)sizeof(uint32_t),false,NULL,0);
    CHECK(oc!=NULL,"mkOc truncated image returned NULL");
    if (oc==NULL) { host_free(&owner_host,empty); return; }
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc)==0 && owner.error_status==LL_E_FORMAT,
          "truncated fixed header was not rejected by ll_init_object");
    CHECK(oc->info==NULL,"truncated header reached ocInit_MachO");
    ll_discard_object(oc);
    check_delta(before,"truncated header leaked or changed owner state");
}

static void load_fixture_case(const char *label, const char *path,
                              bool load_symbols, bool check_aligned_zero)
{
    size_t size=0;
    unsigned char *original=read_image(path,&size);
    CHECK(original!=NULL,"could not read compiled fixture");
    if (original==NULL) return;
    run_case_checked(label,original,size,NULL,true,load_symbols,check_aligned_zero);
    free(original);
}

static void mutate_cases(const char *ordinary, const char *common)
{
    size_t size=0;
    unsigned char *image=read_image(ordinary,&size);
    CHECK(image!=NULL,"could not read ordinary fixture for mutations");
    if (image!=NULL) {
        run_case("bad-magic",image,size,bad_magic,false,false);
        run_case("bad-command-size-zero",image,size,bad_cmdsize,false,false);
        run_case("bad-command-size-large",image,size,huge_cmdsize,false,false);
        run_case("bad-command-count",image,size,huge_command_count,false,false);
        run_case("bad-command-bytes",image,size,bad_command_bytes,false,false);
        run_case("bad-section-count",image,size,huge_section_count,false,false);
        run_case("bad-symbol-table-offset",image,size,bad_symoff,false,false);
        run_case("bad-symbol-count",image,size,huge_symbol_count,false,false);
        run_case("bad-string-table-span",image,size,bad_stroff,false,false);
        run_case("bad-string-index",image,size,bad_string_index,false,false);
        run_case("unterminated-symbol-name",image,size,unterminated_symbol_name,false,false);
        run_case("bad-symbol-section-index",image,size,bad_symbol_section,false,false);
        run_case("bad-section-payload",image,size,bad_payload,false,false);
        run_case("bad-section-alignment",image,size,bad_alignment,false,false);
        run_case("bad-relocation-table-span",image,size,bad_reloc_span,false,false);
        run_case("bad-relocation-count",image,size,huge_reloc_count,false,false);
        run_case("bad-relocation-symbol-index",image,size,bad_reloc_symbol,false,false);
        run_case("bad-relocation-operand-span",image,size,bad_reloc_operand,false,false);
        run_case("bad-init-array-shape",image,size,bad_init_shape,false,false);
        run_case("valid-sixteen-byte-fixed-names",image,size,unterminated_fixed_names,true,true);
        free(image);
    }
    size=0;
    image=read_image(common,&size);
    CHECK(image!=NULL,"could not read COMMON fixture for mutation");
    if (image!=NULL) {
        run_case("common-single-oversize",image,size,common_overflow,false,false);
        run_case("common-sum-overflow",image,size,common_sum_overflow,false,false);
        free(image);
    }
}

int main(int argc, char **argv)
{
    if (argc!=4) {
        (void)fprintf(stderr,"usage: macho_input ordinary.o common-only.o zero-size.o\n");
        return 2;
    }
    ll_compat_host_ops default_ops=make_ops(&default_host);
    ll_compat_host_ops owner_ops=make_ops(&owner_host);
    ll_install_host_for(&ll_default_linker,&default_ops,NULL);
    ll_install_host_for(&owner,&owner_ops,NULL);
    ll_default_linker.error_status=LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message,default_sentinel,sizeof(default_sentinel));
    ll_init_linker(&owner,0);
    owner.services.cfg.always_pic=true;

    run_empty_image();
    load_fixture_case("ordinary-positive",argv[1],true,true);
    load_fixture_case("common-only-positive",argv[2],true,false);
    load_fixture_case("zero-size-positive",argv[3],true,false);
    mutate_cases(argv[1],argv[2]);

    CHECK(owner_host.fatals==0,"owner fatal callback was called");
    CHECK(default_host.allocs==0 && default_host.frees==0
              && default_host.logs==0 && default_host.fatals==0,
          "test changed default linker host callbacks");
    CHECK(ll_default_linker.error_status==LL_E_NOT_FOUND
              && strcmp(ll_default_linker.error_message,default_sentinel)==0,
          "test changed default linker error sentinel");
    ll_exit_linker(&owner);
    CHECK(owner_host.live==0,"linker exit left test-owner allocations live");
    if (failures!=0) return 1;
    (void)puts("macho_input: PASS");
    return 0;
}
