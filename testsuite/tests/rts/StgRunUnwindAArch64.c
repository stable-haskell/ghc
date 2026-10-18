/* Exercise C -> synthetic STG -> unsafe C, then unwind through the bottom
 * STG frame and StgRunJmp. The synthetic frame uses the NCG's machine-SP rule
 * while deliberately leaving x29 at the unsafe foreign-call frame.
 * Checking the restored caller SP catches a boundary CFA based on stale x29;
 * checking only return PCs or register preservation would miss that defect.
 */
#include "Rts.h"
#include <stdint.h>
#include <stdio.h>
#include <unwind.h>

StgRegTable *stgUnwindCaller(StgRegTable *);
extern char stgUnwindCallerReturn[];
uintptr_t stgUnwindExpectedSp;
static unsigned seen;
static unsigned failures;

static _Unwind_Reason_Code visit(struct _Unwind_Context *context, void *arg)
{
    (void)arg;
    uintptr_t ip = (uintptr_t)_Unwind_GetIP(context);
    uintptr_t sp = (uintptr_t)_Unwind_GetGR(context, 31);
    if (ip == (uintptr_t)stgUnwindCallerReturn) {
        seen++;
        if (sp != stgUnwindExpectedSp) {
            fprintf(stderr, "STG boundary restored SP %lx; expected %lx\n",
                    (unsigned long)sp, (unsigned long)stgUnwindExpectedSp);
            failures++;
        }
        return _URC_END_OF_STACK;
    }
    return _URC_NO_REASON;
}
void stgUnwindVisit(void)
{
    _Unwind_Backtrace(visit, NULL);
}
int main(void)
{
    StgRegTable regs = {0};
    stgUnwindCaller(&regs);
    if (seen != 1) {
        fprintf(stderr, "Expected one caller frame, saw %u\n", seen);
    }
    return seen == 1 && failures == 0 ? 0 : 1;
}
