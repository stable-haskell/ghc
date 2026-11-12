#pragma once

#if defined(OBJFORMAT_JS)

/* No native sections. Pointer-sized placeholders keep the shared object layout. */
struct SectionFormatInfo { };

struct ObjectCodeFormatInfo { };

#endif
