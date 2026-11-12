/* Explicit arrays cover the section path separately from __cxa_atexit. */
extern void fixture_record_event(unsigned event);

static void ll_fixture_array_init(void)
{
    fixture_record_event(3);
}

static void ll_fixture_array_fini(void)
{
    fixture_record_event(4);
}

typedef void (*ll_fixture_hook)(void);

__attribute__((used, section("__DATA,__mod_init_func,mod_init_funcs")))
static ll_fixture_hook const ll_fixture_init_array = ll_fixture_array_init;

__attribute__((used, section("__DATA,__mod_term_func,mod_term_funcs")))
static ll_fixture_hook const ll_fixture_fini_array = ll_fixture_array_fini;
