extern void fixture_record_event(unsigned int);

static void fixture_legacy_ctor(void)
{
    fixture_record_event(4u);
}

void (*fixture_legacy_ctor_entry)(void) __attribute__((section(".ctors"))) = fixture_legacy_ctor;
