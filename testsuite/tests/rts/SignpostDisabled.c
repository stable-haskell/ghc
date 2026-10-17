#include "Signpost.h"
#include <assert.h>
static unsigned calls;
#define ARG (++calls)
int main(void)
{
    initSignposts();
    assert(signpostGcBegin(ARG, ARG) == 0);
    signpostGcEnd(ARG, ARG, ARG);
    signpostThreadCreate(ARG, ARG);
    signpostThreadRun(ARG, ARG);
    signpostThreadStop(ARG, ARG, ARG);
    signpostUserMsg(ARG, ARG);
    signpostUserMarker(ARG, ARG);
    assert(calls == 0);
    return 0;
}
