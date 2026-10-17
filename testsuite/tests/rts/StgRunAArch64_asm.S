/* Check callee-save registers across a C -> STG -> C round trip. */
#if defined(__APPLE__)
#define NAME(x) _##x
#else
#define NAME(x) x
#endif
        .text
        .balign 4
        .globl NAME(stgRunAArch64Probe)
NAME(stgRunAArch64Probe):
        .cfi_startproc
        stp x29, x30, [sp, #-160]!
        .cfi_def_cfa_offset 160
        .cfi_offset 29, -160
        .cfi_offset 30, -152
        mov x29, sp
        stp x19, x20, [sp, #16]
        stp x21, x22, [sp, #32]
        stp x23, x24, [sp, #48]
        stp x25, x26, [sp, #64]
        stp x27, x28, [sp, #80]
        stp d8, d9, [sp, #96]
        stp d10, d11, [sp, #112]
        stp d12, d13, [sp, #128]
        stp d14, d15, [sp, #144]
        mov x19, #19
        mov x20, #20
        mov x21, #21
        mov x22, #22
        mov x23, #23
        mov x24, #24
        mov x25, #25
        mov x26, #26
        mov x27, #27
        mov x28, #28
        mov x9, #8
        fmov d8, x9
        mov x9, #9
        fmov d9, x9
        mov x9, #10
        fmov d10, x9
        mov x9, #11
        fmov d11, x9
        mov x9, #12
        fmov d12, x9
        mov x9, #13
        fmov d13, x9
        mov x9, #14
        fmov d14, x9
        mov x9, #15
        fmov d15, x9
        bl NAME(stgRunAArch64Callback)
        cmp w0, #45
        b.ne 1f
        cmp x19, #19
        b.ne 1f
        cmp x20, #20
        b.ne 1f
        cmp x21, #21
        b.ne 1f
        cmp x22, #22
        b.ne 1f
        cmp x23, #23
        b.ne 1f
        cmp x24, #24
        b.ne 1f
        cmp x25, #25
        b.ne 1f
        cmp x26, #26
        b.ne 1f
        cmp x27, #27
        b.ne 1f
        cmp x28, #28
        b.ne 1f
        fmov x9, d8
        cmp x9, #8
        b.ne 1f
        fmov x9, d9
        cmp x9, #9
        b.ne 1f
        fmov x9, d10
        cmp x9, #10
        b.ne 1f
        fmov x9, d11
        cmp x9, #11
        b.ne 1f
        fmov x9, d12
        cmp x9, #12
        b.ne 1f
        fmov x9, d13
        cmp x9, #13
        b.ne 1f
        fmov x9, d14
        cmp x9, #14
        b.ne 1f
        fmov x9, d15
        cmp x9, #15
        b.ne 1f
        mov w0, #0
        b 2f
1:      mov w0, #1
2:
        ldp x19, x20, [sp, #16]
        ldp x21, x22, [sp, #32]
        ldp x23, x24, [sp, #48]
        ldp x25, x26, [sp, #64]
        ldp x27, x28, [sp, #80]
        ldp d8, d9, [sp, #96]
        ldp d10, d11, [sp, #112]
        ldp d12, d13, [sp, #128]
        ldp d14, d15, [sp, #144]
        ldp x29, x30, [sp], #160
        .cfi_def_cfa_offset 0
        .cfi_restore 29
        .cfi_restore 30
        ret
        .cfi_endproc
#if !defined(__APPLE__)
        .section .note.GNU-stack,"",%progbits
#endif
