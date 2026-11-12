extern int __cxa_atexit(void (*)(void *), void *, void *);
extern void *__dso_handle;
extern void fixture_record_event(unsigned int);

static void fixture_cxa_fini(void *unused)
{
    (void)unused;
    fixture_record_event(3u);
}

__attribute__((constructor)) static void fixture_init(void)
{
    fixture_record_event(1u);
    (void)__cxa_atexit(fixture_cxa_fini, 0, &__dso_handle);
}

__attribute__((destructor)) static void fixture_destructor(void)
{
    fixture_record_event(2u);
}
