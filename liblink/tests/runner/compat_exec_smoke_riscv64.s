/* Linux/RISC-V fixture for compat_exec_smoke.c; uses CALL_PLT, no HI/LO pair. */
.option norelax
.text
.p2align 2
.globl fixture_entry
fixture_entry:
    addi sp, sp, -16
    sd ra, 8(sp)
    call external_callback
    ld ra, 8(sp)
    addi sp, sp, 16
    ret
