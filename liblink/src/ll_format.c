/* Native compatibility adapters for the frozen format interface. Runtime
 * selection is independent of host byte order and never interprets structures.
 * Backend portability and a multi-backend registry follow in WP2.6/2.7. */
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "ll_format.h"

static bool ll_sniff_elf(const uint8_t *img, size_t len)
{
    return img != NULL && len >= 4 && img[0] == 0x7f &&
           img[1] == 'E' && img[2] == 'L' && img[3] == 'F';
}

static bool ll_sniff_macho(const uint8_t *img, size_t len)
{
    if (img == NULL || len < 4) return false;
    /* MH_MAGIC[_64] and MH_CIGAM[_64], read as bytes on either host. */
    return (img[0] == 0xfe && img[1] == 0xed && img[2] == 0xfa &&
            (img[3] == 0xce || img[3] == 0xcf)) ||
           ((img[0] == 0xce || img[0] == 0xcf) && img[1] == 0xfa &&
            img[2] == 0xed && img[3] == 0xfe);
}

static bool ll_sniff_coff(const uint8_t *img, size_t len)
{
    if (img == NULL || len < 4) return false;
    /* Anonymous COFF (including bigobj and short import objects). */
    if (img[0] == 0 && img[1] == 0 && img[2] == 0xff && img[3] == 0xff)
        return true;
    /* Ordinary COFF has no magic. Recognize its machine field only with a
     * complete fixed header. Architecture support is the verifier's decision. */
    if (len < 20) return false;
    const unsigned machine = (unsigned)img[0] | ((unsigned)img[1] << 8);
    switch (machine) {
    case 0x014c: /* I386 */
    case 0x01c0: /* ARM */
    case 0x01c2: /* THUMB */
    case 0x01c4: /* ARMNT */
    case 0x0200: /* IA64 */
    case 0x8664: /* AMD64 */
    case 0xaa64: /* ARM64 */
        return true;
    default:
        return false;
    }
}

static bool ll_sniff_wasm(const uint8_t *img, size_t len)
{
    return img != NULL && len >= 4 && img[0] == 0 &&
           img[1] == 'a' && img[2] == 's' && img[3] == 'm';
}

#if defined(OBJFORMAT_ELF)
#include "linker/Elf.h"
static int ll_native_init(ObjectCode *oc)
{
    /* Preserve the existing pre-init verification and the loadOc recheck. */
    if (!ocVerifyImage_ELF(oc)) return 0;
    ocInit_ELF(oc);
    return 1;
}
static int ll_native_extras(ObjectCode *oc)
{
#if defined(NEED_SYMBOL_EXTRAS)
    return ocAllocateExtras_ELF(oc);
#else
    (void)oc;
    return 1;
#endif
}
static const struct ll_format_ops ll_native_ops = {
    .name = "ELF", .sniff = ll_sniff_elf,
    .verify = ocVerifyImage_ELF, .init = ll_native_init,
    .alloc_extras = ll_native_extras, .get_names = ocGetNames_ELF,
    .resolve = ocResolve_ELF, .run_init = ocRunInit_ELF,
    .run_fini = ocRunFini_ELF, .deinit = ocDeinit_ELF
};
#elif defined(OBJFORMAT_MACHO)
#include "linker/MachO.h"
static int ll_native_init(ObjectCode *oc)
{
    if (!ocVerifyImage_MachO(oc)) return 0;
    return ocInit_MachO(oc);
}
static int ll_native_extras(ObjectCode *oc)
{
#if defined(NEED_SYMBOL_EXTRAS)
    return ocAllocateExtras_MachO(oc);
#else
    (void)oc;
    return 1;
#endif
}
static const struct ll_format_ops ll_native_ops = {
    .name = "Mach-O", .sniff = ll_sniff_macho,
    .verify = ocVerifyImage_MachO, .init = ll_native_init,
    .alloc_extras = ll_native_extras, .get_names = ocGetNames_MachO,
    .resolve = ocResolve_MachO, .run_init = ocRunInit_MachO,
    .run_fini = ocRunFini_MachO, .deinit = ocDeinit_MachO
};
#elif defined(OBJFORMAT_PEi386)
#include "linker/PEi386.h"
/* PE verification allocates metadata. Do not introduce an extra verification
 * at initialization. Its extras are acquired by get_names as before. Use typed
 * wrappers: the backend returns bool, while the frozen slots return int. */
static int ll_native_noop(ObjectCode *oc) { (void)oc; return 1; }
static int ll_native_verify(ObjectCode *oc) { return ocVerifyImage_PEi386(oc); }
static int ll_native_names(ObjectCode *oc) { return ocGetNames_PEi386(oc); }
static int ll_native_resolve(ObjectCode *oc) { return ocResolve_PEi386(oc); }
static int ll_native_run_init(ObjectCode *oc) { return ocRunInit_PEi386(oc); }
static int ll_native_run_fini(ObjectCode *oc) { return ocRunFini_PEi386(oc); }
static const struct ll_format_ops ll_native_ops = {
    .name = "PEi386", .sniff = ll_sniff_coff,
    .verify = ll_native_verify, .init = ll_native_noop,
    .alloc_extras = ll_native_noop, .get_names = ll_native_names,
    .resolve = ll_native_resolve, .run_init = ll_native_run_init,
    .run_fini = ll_native_run_fini, .deinit = freePreloadObjectFile_PEi386
};
#endif

const struct ll_format_ops *ll_format_native(void)
{
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO) || defined(OBJFORMAT_PEi386)
    return &ll_native_ops;
#else
    return NULL;
#endif
}

const struct ll_format_ops *ll_format_select(ObjectCode *oc)
{
    const uint8_t *img = (const uint8_t *)oc->image;
    const size_t len = oc->fileSize > 0 ? (size_t)oc->fileSize : 0;
    const struct ll_format_ops *native = ll_format_native();
    if (native != NULL && native->sniff(img, len)) return native;

    const char *name = ll_sniff_elf(img, len) ? "ELF" :
                       ll_sniff_macho(img, len) ? "Mach-O" :
                       ll_sniff_coff(img, len) ? "COFF" :
                       ll_sniff_wasm(img, len) ? "Wasm" : NULL;
    if (name != NULL)
        ll_error_set(oc->L, LL_E_UNSUPPORTED,
                     "loadObj: %s backend is unavailable for %" PATH_FMT,
                     name, OC_INFORMATIVE_FILENAME(oc));
    else
        ll_error_set(oc->L, LL_E_FORMAT,
                     "loadObj: unrecognized object format in %" PATH_FMT,
                     OC_INFORMATIVE_FILENAME(oc));
    return NULL;
}

int ll_format_require(ObjectCode *oc)
{
    if (oc->format_ops != NULL) return 1;
    ll_error_set(oc->L, LL_E_STATE, "object format is not initialized for %" PATH_FMT,
                 OC_INFORMATIVE_FILENAME(oc));
    return 0;
}
