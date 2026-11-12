extern void fixture_record_event(unsigned event);

__attribute__((constructor))
static void ll_fixture_init(void)
{
    fixture_record_event(1);
}

__attribute__((destructor))
static void ll_fixture_fini(void)
{
    fixture_record_event(2);
}
