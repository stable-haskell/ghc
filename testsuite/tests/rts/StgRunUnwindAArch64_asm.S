#if defined(__APPLE__)
#define NAME(x) _##x
#define LOAD_PAGE(r, sym) adrp r, NAME(sym)@PAGE
#define LOAD_OFFSET(r, sym) add r, r, NAME(sym)@PAGEOFF
#else
#define NAME(x) x
#define LOAD_PAGE(r, sym) adrp r, NAME(sym)
#define LOAD_OFFSET(r, sym) add r, r, :lo12:NAME(sym)
#endif
        .text
        .balign 4
        .globl NAME(stgUnwindCaller)
        .globl NAME(stgUnwindCallerReturn)
#if defined(__APPLE__)
        .alt_entry NAME(stgUnwindCallerReturn)
#endif
NAME(stgUnwindCaller):
        .cfi_startproc
        stp x29, x30, [sp, #-16]!
        .cfi_def_cfa_offset 16
        .cfi_offset 29, -16
        .cfi_offset 30, -8
        mov x29, sp
        .cfi_def_cfa_register 29
        mov x9, sp
        LOAD_PAGE(x10, stgUnwindExpectedSp)
        LOAD_OFFSET(x10, stgUnwindExpectedSp)
        str x9, [x10]
        mov x1, x0
        LOAD_PAGE(x0, stgUnwindSynthetic)
        LOAD_OFFSET(x0, stgUnwindSynthetic)
        bl NAME(StgRun)
NAME(stgUnwindCallerReturn):
        ldp x29, x30, [sp], #16
        .cfi_def_cfa 31, 0
        .cfi_restore 29
        .cfi_restore 30
        ret
        .cfi_endproc

        .globl NAME(stgUnwindSynthetic)
NAME(stgUnwindSynthetic):
        .cfi_startproc
        mov x20, x19
        LOAD_PAGE(x21, StgRunJmp)
        LOAD_OFFSET(x21, StgRunJmp)
        .cfi_def_cfa 20, 0
        .cfi_register 30, 21
        .cfi_same_value 29
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        /* NCG MachSp unwind: caller machine SP = current SP + 16. */
        .cfi_escape 0x16, 31, 2, 0x8f, 16
        bl NAME(stgUnwindVisit)
        ldp x29, x30, [sp], #16
        mov x22, #0
        b NAME(StgReturn)
        .cfi_endproc

#if !defined(__APPLE__)
        .section .note.GNU-stack,"",%progbits
#endif
