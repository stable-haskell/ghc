.syntax unified
.arm
.text
.globl fixture_arm_movt
fixture_arm_movt:
  movt r0, #:upper16:external_data
