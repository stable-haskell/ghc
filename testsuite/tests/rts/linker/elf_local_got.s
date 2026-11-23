// Exercise GOT references to named local data, functions and STT_NOTYPE.
// Keep unrelated local TLS/absolute symbols: they must not cause fillGot to
// demand unsupported addresses when no relocation uses them.
// Assemble the referenced definitions as global, then localize them with
// objcopy. Older assemblers otherwise fold their GOT relocations to section
// symbols plus nonzero addends, losing the named-local coverage.

        .data
        .p2align 3
        .global local_data
        .type local_data, %object
local_data:
        .word 17
        .size local_data, .-local_data

        .global local_notype
local_notype:
        .word 5

        .local local_absolute
        .type local_absolute, %object
        .set local_absolute, 123

        .section .tbss,"awT",%nobits
        .p2align 3
        .local local_tls
        .type local_tls, %tls_object
local_tls:
        .zero 8
        .size local_tls, .-local_tls

        .text
        .p2align 2
        .global local_function
        .type local_function, %function
local_function:
        mov w0, #25
        ret
        .size local_function, .-local_function

        .global elf_local_got_entry
        .type elf_local_got_entry, %function
elf_local_got_entry:
        stp x19, x30, [sp, #-16]!
        adrp x9, :got:local_data
        ldr x9, [x9, :got_lo12:local_data]
        ldr w19, [x9]
        adrp x9, :got:local_function
        ldr x9, [x9, :got_lo12:local_function]
        blr x9
        add w19, w19, w0
        adrp x9, :got:local_notype
        ldr x9, [x9, :got_lo12:local_notype]
        ldr w9, [x9]
        add w0, w19, w9
        ldp x19, x30, [sp], #16
        ret
        .size elf_local_got_entry, .-elf_local_got_entry

        .section .note.GNU-stack,"",%progbits
