.text
.globl fixture_i386_gotpc
fixture_i386_gotpc:
  calll .Lpic
.Lpic:
  popl %ebx
  addl $_GLOBAL_OFFSET_TABLE_+(.-.Lpic), %ebx
  retl
