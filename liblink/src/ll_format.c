/* Immutable object-format registry and portable bounded signature recognition.
 * Backend implementations own their typed operation tables. */
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "ll_format.h"

bool ll_format_sniff_elf(const uint8_t *img, size_t len)
{
    return img != NULL && len >= 4 && img[0] == 0x7f &&
           img[1] == 'E' && img[2] == 'L' && img[3] == 'F';
}

bool ll_format_sniff_macho(const uint8_t *img, size_t len)
{
    if (img == NULL || len < 4) return false;
    /* MH_MAGIC[_64] and MH_CIGAM[_64], read as bytes on either host. */
    return (img[0] == 0xfe && img[1] == 0xed && img[2] == 0xfa &&
            (img[3] == 0xce || img[3] == 0xcf)) ||
           ((img[0] == 0xce || img[0] == 0xcf) && img[1] == 0xfa &&
            img[2] == 0xed && img[3] == 0xfe);
}

bool ll_format_sniff_coff(const uint8_t *img, size_t len)
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

bool ll_format_sniff_wasm(const uint8_t *img, size_t len)
{
    return img != NULL && len >= 4 && img[0] == 0 &&
           img[1] == 'a' && img[2] == 's' && img[3] == 'm';
}

/* Only compiled backends enter this immutable registry. Portable recognition
 * below still identifies known formats whose backend is unavailable. */
static const struct ll_format_ops *const ll_formats[] = {
#if defined(OBJFORMAT_ELF)
    &ll_format_elf,
#endif
#if defined(OBJFORMAT_MACHO)
    &ll_format_macho,
#endif
#if defined(OBJFORMAT_PEi386)
    &ll_format_pei386,
#endif
    NULL
};

const struct ll_format_ops *ll_format_native(void)
{
    /* Current builds contain one native implementation; Wasm contains none. */
    return ll_formats[0];
}

const struct ll_format_ops *ll_format_select(ObjectCode *oc)
{
    const uint8_t *img = (const uint8_t *)oc->image;
    const size_t len = oc->fileSize > 0 ? (size_t)oc->fileSize : 0;
    for (size_t i = 0; ll_formats[i] != NULL; ++i) {
        const struct ll_format_ops *ops = ll_formats[i];
        if (ops->sniff(img, len)) return ops;
    }

    const char *name = ll_format_sniff_elf(img, len) ? "ELF" :
                       ll_format_sniff_macho(img, len) ? "Mach-O" :
                       ll_format_sniff_coff(img, len) ? "COFF" :
                       ll_format_sniff_wasm(img, len) ? "Wasm" : NULL;
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
