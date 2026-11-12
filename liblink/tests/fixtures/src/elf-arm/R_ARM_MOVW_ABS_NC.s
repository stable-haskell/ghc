.syntax unified
.arm
.text
.globl fixture_arm_movw
fixture_arm_movw:
  movw r0, #:lower16:external_data
