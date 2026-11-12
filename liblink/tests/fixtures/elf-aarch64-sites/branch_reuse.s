# Data-only branch and veneer identity controls; never executed.
.text
.globl fixture_aarch64_branch_reuse
fixture_aarch64_branch_reuse:
bl external_function
bl external_function
bl external_function_alias
bl external_second_function
.section .note.GNU-stack,"",@progbits
