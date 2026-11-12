#include <windows.h>

static BOOLEAN NTAPI test_delete_function_table(PRUNTIME_FUNCTION table);

/* Reuse the Windows PE fixture ledger and real included cleanup functions. */
#define main pei386_common_ownership_main
#define PEI386_TEST_RTLDELETE test_delete_function_table
#include "pei386_common_ownership.c"
#undef main
#undef PEI386_TEST_RTLDELETE

static PRUNTIME_FUNCTION disposed_table;
static unsigned dispose_calls;
static uintptr_t retained_info_address;
static bool callback_saw_live_shell;

static BOOLEAN NTAPI test_delete_function_table(PRUNTIME_FUNCTION table)
{
    disposed_table = table;
    ++dispose_calls;
    for (size_t i = 0; i < fixture.allocation_count; ++i)
        if ((uintptr_t)fixture.allocations[i] == retained_info_address)
            callback_saw_live_shell = true;
    return TRUE;
}

static void test_retained_table_lifetime(void)
{
    ObjectCode oc = {0};
    Section pdata = {0};
    const uintptr_t marker = UINT64_C(0x12345678);
    oc.L = &owner;
    pdata.start = (void *)marker;
    oc.info = LL_CALLOC_FOR(&owner, sizeof(*oc.info), 1,
                            "unwind retention test info");
    oc.info->pdata = &pdata;
    oc.info->xdata = &pdata;
    oc.info->ch_info = LL_CALLOC_FOR(&owner, sizeof(*oc.info->ch_info), 1,
                                     "unwind retention test header");
    oc.info->symbols = LL_CALLOC_FOR(&owner, sizeof(*oc.info->symbols), 1,
                                     "unwind retention test symbols");
    oc.info->str_tab = LL_CALLOC_FOR(&owner, 1, 1,
                                     "unwind retention test strings");
    retained_info_address = (uintptr_t)oc.info;

    releaseOcInfo(&oc);
    CHECK(oc.info != NULL,
          "failed resolution discarded the live unwind ownership record");
    if (oc.info != NULL) {
        CHECK(oc.info->pdata == &pdata && oc.info->xdata == &pdata,
              "failed resolution changed the live unwind table references");
        CHECK(oc.info->ch_info == NULL && oc.info->symbols == NULL
                  && oc.info->str_tab == NULL,
              "failed resolution retained disposable parse metadata");
        releaseOcInfo(&oc);
        CHECK(oc.info != NULL && oc.info->pdata == &pdata,
              "repeated failed cleanup discarded the live unwind record");
        freePreloadObjectFile_PEi386(&oc);
    }
    CHECK(oc.info == NULL,
          "physical disposal did not release the retained format record");
    CHECK(dispose_calls == 1 && disposed_table == (PRUNTIME_FUNCTION)marker,
          "physical disposal did not unregister the exact retained table");
    CHECK(callback_saw_live_shell,
          "physical-disposal callback ran after the ownership shell was freed");
    bool shell_still_live = false;
    for (size_t i = 0; i < fixture.allocation_count; ++i)
        if ((uintptr_t)fixture.allocations[i] == retained_info_address)
            shell_still_live = true;
    CHECK(!shell_still_live,
          "physical disposal did not free the retained ownership shell");

    /* No registered unwind table preserves the existing eager cleanup path. */
    oc.info = LL_CALLOC_FOR(&owner, sizeof(*oc.info), 1,
                            "unwind no-table test info");
    releaseOcInfo(&oc);
    CHECK(oc.info == NULL,
          "failed resolution retained an empty format record without pdata");
    CHECK(dispose_calls == 1,
          "an object without pdata invoked the disposal callback");
}

int main(void)
{
    install_owner();
    test_retained_table_lifetime();
    CHECK(fixture.fatal_calls == 0, "linker fatal callback was called");
    ll_exit_linker(&owner);
    release_test_linker_auxiliary_state();
    CHECK(fixture.allocation_count == 0,
          "unwind ownership unit left host allocations live after cleanup");
    while (fixture.allocation_count != 0) {
        void *allocation = fixture.allocations[--fixture.allocation_count];
        free(allocation);
    }
    if (failures != 0) return 1;
    (void)puts("pei386_unwind_retention: PASS");
    return 0;
}
