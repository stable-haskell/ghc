int wp24_macho_data = 17;
_Alignas(32768) int wp24_macho_aligned_zero
    __attribute__((section("__DATA,__bss")));
extern int wp24_macho_external;
int *wp24_macho_external_pointer = &wp24_macho_external;

int wp24_macho_entry(void)
{
    return wp24_macho_data + wp24_macho_external;
}

static void wp24_macho_init(void) __attribute__((constructor));
static void wp24_macho_fini(void) __attribute__((destructor));
static void wp24_macho_init(void) { }
static void wp24_macho_fini(void) { }
static void (*const wp24_macho_init_slot)(void)
    __attribute__((section("__DATA,__mod_init_func"), used)) = wp24_macho_init;
static void (*const wp24_macho_fini_slot)(void)
    __attribute__((section("__DATA,__mod_term_func"), used)) = wp24_macho_fini;
