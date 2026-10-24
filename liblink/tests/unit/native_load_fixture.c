/* Data-only success fixture; the optional unresolved function is never called. */
int ll_owner_fixture_value = 73;
#ifdef NATIVE_LOAD_LAZY_FIXTURE
extern int ll_native_test_missing_symbol(void);
int ll_native_unused_reference(void)
{
    return ll_native_test_missing_symbol();
}
#endif
