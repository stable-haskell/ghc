/* The object is inspected and relocated but never executed by portable tests. */
extern void arm64_target(void);
extern int arm64_data;
void arm64_call(void) { arm64_target(); }
int *arm64_address(void) { return &arm64_data; }
void (*arm64_pointer)(void) = arm64_target;
