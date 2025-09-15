/*
These utility routines are used various
places in the GHC library.
*/

#include <Rts.h>

#include <HsFFI.h>

/* Prototype for FFI-callable helper */
void enableTimingStats( void );
void setHeapSize( HsInt size );
void setGhciPrelinkArchiveThreshold( HsInt64 bytes );

void
enableTimingStats( void )       /* called from the driver */
{
    RtsFlags.GcFlags.giveStats = ONELINE_GC_STATS;
}

void
setHeapSize( HsInt size )
{
    RtsFlags.GcFlags.heapSizeSuggestion = size / BLOCK_SIZE;
    if (RtsFlags.GcFlags.maxHeapSize != 0 &&
        RtsFlags.GcFlags.heapSizeSuggestion > RtsFlags.GcFlags.maxHeapSize) {
        RtsFlags.GcFlags.maxHeapSize = RtsFlags.GcFlags.heapSizeSuggestion;
    }
}

/* Configure GHCi pre-link archive threshold (in bytes). 0 disables. */
void
setGhciPrelinkArchiveThreshold( HsInt64 bytes )
{
    RtsFlags.MiscFlags.linkerPrelinkArchiveThreshold = (StgInt64) bytes;
}
