// Deliberately omit dllimport: a direct function reference requires the RTS
// linker to retain its code type and construct a jump island when necessary.
extern int bfd_imports_function(void);
extern int bfd_imports_data;

int bfd_imports_entry(void)
{
    return bfd_imports_function() + bfd_imports_data;
}
