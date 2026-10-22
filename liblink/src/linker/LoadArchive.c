#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"

#include "ll_path.h"

#include "LinkerInternals.h"
#include "linker/M32Alloc.h"
#include "linker/MMap.h"

/* Platform specific headers */
#if defined(OBJFORMAT_PEi386)
#  include "linker/PEi386.h"
#elif defined(OBJFORMAT_MACHO)
#  include "linker/MachO.h"
#  include <regex.h>
#  include <mach/machine.h>
#  include <mach-o/fat.h>
#elif defined(OBJFORMAT_ELF)
#include "linker/Elf.h"
#endif

#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include "ll_fs.h"

#define FAIL(...) do {\
   LL_LOG_ERROR("loadArchive: "__VA_ARGS__); \
   goto fail;\
} while (0)

#define DEBUG_LOG(...) LL_IF_DEBUG(linker, LL_DEBUG("loadArchive: " __VA_ARGS__))


#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
/* Read 4 bytes and convert to host byte order */
static uint32_t read4Bytes(const char buf[static 4])
{
    return ntohl(*(uint32_t*)buf);
}

static bool loadFatArchive(char input[static 20], FILE* f, pathchar* path)
{
    uint32_t nfat_arch, nfat_offset, cputype, cpusubtype;
#if defined(i386_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_X86;
    const uint32_t mycpusubtype = CPU_SUBTYPE_X86_ALL;
#elif defined(x86_64_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_X86_64;
    const uint32_t mycpusubtype = CPU_SUBTYPE_X86_64_ALL;
#elif defined(aarch64_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_ARM64;
    const uint32_t mycpusubtype = CPU_SUBTYPE_ARM64_ALL;
#elif defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH)
#error No Darwin support on PowerPC
#else
#error Unknown Darwin architecture
#endif

    nfat_arch = read4Bytes(input + 4);
    DEBUG_LOG("found a fat archive containing %d architectures\n", nfat_arch);
    char tmp[20];
    nfat_offset = 0;
    for (uint32_t i = 0; i < nfat_arch; i++) {
        /* search for the right arch */
        int n = fread(tmp, 1, 12, f);
        if (n != 12) {
            LL_LOG_ERROR("Failed reading arch from `%" PATH_FMT "'", path);
            return false;
        }
        cputype = read4Bytes(tmp);
        cpusubtype = read4Bytes(tmp + 4);
        if (cputype == mycputype && cpusubtype == mycpusubtype) {
            DEBUG_LOG("found my archive in a fat archive\n");
            nfat_offset = read4Bytes(tmp + 8);
            break;
        }
    }
    if (nfat_offset == 0) {
        LL_LOG_ERROR("Fat archive contains %d architectures, "
                   "but none of them are compatible with the host",
                   (int)nfat_arch);
        return false;
    } else {
        /* Seek to the correct architecture */
        int n = fseek(f, nfat_offset, SEEK_SET);
        if (n != 0) {
            LL_LOG_ERROR("Failed to seek to arch in `%" PATH_FMT "'", path);
            return false;
        }

        /* Read the header */
        char tmp[20];
        n = fread(tmp, 1, 8, f);
        if (n != 8) {
            LL_LOG_ERROR("Failed reading header from `%" PATH_FMT "'", path);
            return false;
        }

        /* Check the magic number */
        if (strncmp(tmp, "!<arch>\n", 8) != 0) {
            LL_LOG_ERROR("couldn't find archive in `%" PATH_FMT "'"
                       "at offset %d", path, nfat_offset);
            return false;
        }
    }
    return true;
}
#endif

enum ObjectFileFormat {
    NotObject,
    COFFAmd64,
    COFFI386,
    COFFAArch64,
    ELF,
    MachO32,
    MachO64,
};

static enum ObjectFileFormat identifyObjectFile_(char* buf, size_t sz)
{
    if (sz > 2 && ((uint16_t*)buf)[0] == 0x8664) {
        return COFFAmd64;
    }
    if (sz > 2 && ((uint16_t*)buf)[0] == 0x014c) {
        return COFFI386;
    }
    if (sz > 2 && ((uint16_t*)buf)[0] == 0xaa64) {
        return COFFAArch64;
    }
    if (sz > 4 && memcmp(buf, "\x7f" "ELF", 4) == 0) {
        return ELF;
    }
    if (sz > 4 && ((uint32_t*)buf)[0] == 0xfeedface) {
        return MachO32;
    }
    if (sz > 4 && ((uint32_t*)buf)[0] == 0xfeedfacf) {
        return MachO64;
    }
    // BigObj COFF files ...
    if (sz > 8 && ((uint64_t*)buf)[0] == 0x86640002ffff0000) {
        return COFFAmd64;
    }
    return NotObject;
}

static enum ObjectFileFormat identifyObjectFile(FILE *f)
{
    char buf[32];
    ssize_t sz = fread(buf, 1, 32, f);
    LL_CHECK(fseek(f, -sz, SEEK_CUR) == 0);
    return identifyObjectFile_(buf, sz);
}

static bool readThinArchiveMember(int n, int memberSize, pathchar* path,
        char* fileName, char* image)
{
    bool has_succeeded = false;
    FILE* member = NULL;
    pathchar *pathCopy, *dirName, *memberPath, *objFileName;
    memberPath = NULL;
    /* Allocate and setup the dirname of the archive.  We'll need
     this to locate the thin member */
    pathCopy = pathdup(&ll_default_linker, path); // Convert the char* to a pathchar*
    dirName = pathdir(&ll_default_linker, pathCopy);
    /* Append the relative member name to the dirname.  This should be
     be the full path to the actual thin member. */
    int memberLen = pathlen(dirName) + 1 + strlen(fileName) + 1;
    memberPath = LL_MALLOC(pathsize * memberLen, "loadArchive(file)");
    objFileName = mkPath(&ll_default_linker, fileName);
    pathprintf(memberPath, memberLen, WSTR("%" PATH_FMT "%" PATH_FMT), dirName,
            objFileName);
    LL_FREE(objFileName);
    LL_FREE(dirName);
    member = pathopen(memberPath, WSTR("rb"));
    if (!member) {
        LL_LOG_ERROR("loadObj: can't read thin archive `%" PATH_FMT "'",
                   memberPath);
        goto inner_fail;
    }
    n = fread(image, 1, memberSize, member);
    if (n != memberSize) {
        LL_LOG_ERROR("loadArchive: error whilst reading `%s'",
                   fileName);
        goto inner_fail;
    }
    has_succeeded = true;

inner_fail:
    fclose(member);
    LL_FREE(memberPath);
    LL_FREE(pathCopy);
    return has_succeeded;
}

static bool checkFatArchive(char magic[static 4], FILE* f, pathchar* path)
{
    bool success = false;
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
    /* Not a standard archive, look for a fat archive magic number: */
    if (read4Bytes(magic) == FAT_MAGIC)
        success = loadFatArchive(magic, f, path);
    else
        LL_LOG_ERROR("loadArchive: Neither an archive, nor a fat archive: "
                   "`%" PATH_FMT "'", path);
#else
    (void)magic;
    (void)f;
    LL_LOG_ERROR("loadArchive: Not an archive: `%" PATH_FMT "'", path);
#endif
    return success;
}

/**
 * Look up the filename in the GNU-variant index file pointed to by
 * gnuFileIndex.
 * @param fileName_ a pointer to a pointer to the file name to be looked up.
 * The file name must have been allocated with `StgMallocBytes`, and will
 * be reallocated on return; the old value is now _invalid_.
 * @param gnuFileIndexSize The size of the index.
 */
static bool
lookupGNUArchiveIndex(int gnuFileIndexSize, char **fileName_,
    char* gnuFileIndex, pathchar* path, size_t* thisFileNameSize,
    size_t* fileNameSize)
{
    char *fileName = *fileName_;
    if (isdigit(fileName[1])) {
        if (gnuFileIndex == NULL) {
            LL_LOG_ERROR("loadArchive: GNU-variant filename "
                    "without an index while reading from `%" PATH_FMT "'",
                    path);
            return false;
        }

        int n;
        for (n = 2; isdigit(fileName[n]); n++)
            ;

        char *end;
        fileName[n] = '\0';
        n = strtol(fileName + 1, &end, 10);
        if (n < 0 || n > gnuFileIndexSize) {
            LL_LOG_ERROR("loadArchive: GNU-variant filename "
                    "offset %d out of range [0..%d] "
                    "while reading filename from `%" PATH_FMT "'",
                    n, gnuFileIndexSize, path);
            return false;
        }

        // Check that the previous entry ends with the expected
        // end-of-string delimiter.
#if defined(mingw32_HOST_OS)
#define IS_SYMBOL_DELIMITER(STR) (STR =='\n' || STR == '\0')
#else
#define IS_SYMBOL_DELIMITER(STR) (STR =='\n')
#endif
        if (n != 0 && !IS_SYMBOL_DELIMITER(gnuFileIndex[n - 1])) {
            LL_LOG_ERROR("loadArchive: GNU-variant filename offset "
                    "%d invalid (range [0..%d]) while reading "
                    "filename from `%" PATH_FMT "'",
                    n, gnuFileIndexSize, path);
            return false;
        }

        int i;
        for (i = n; !IS_SYMBOL_DELIMITER(gnuFileIndex[i]); i++)
            ;

        size_t FileNameSize = i - n;
        if (FileNameSize >= *fileNameSize) {
            /* Double it to avoid potentially continually
             increasing it by 1 */
            *fileNameSize = FileNameSize * 2;
            *fileName_ = fileName = LL_REALLOC(fileName, *fileNameSize,
                    "loadArchive(fileName)");
        }
        memcpy(fileName, gnuFileIndex + n, FileNameSize);


       /* llvm-ar terminates string table entries with `/\n`. */
       if (fileName[FileNameSize-1] == '/') {
           FileNameSize--;
       }

        fileName[FileNameSize] = '\0';
        *thisFileNameSize = FileNameSize;
    }
    /* Skip 32-bit symbol table ("/" + 15 blank characters)
       and  64-bit symbol table ("/SYM64/" + 9 blank characters) */
    else if (0 == strncmp(fileName + 1, "               ", 15) ||
             0 == strncmp(fileName + 1, "SYM64/         ", 15)) {
        fileName[0] = '\0';
        *thisFileNameSize = 0;
    }
    else {
        LL_LOG_ERROR("loadArchive: invalid GNU-variant filename `%.16s' "
                   "while reading filename from `%" PATH_FMT "'",
                   fileName, path);
        return false;
    }

    return true;
}

enum ArchiveFormat {
    StandardArchive,
    ThinArchive,
    FatArchive,
};

static bool identifyArchiveFormat(FILE *f, pathchar *path, enum ArchiveFormat *out)
{
    char tmp[8];
    size_t n = fread(tmp, 1, 8, f);
    if (n != 8) {
        LL_LOG_ERROR("loadArchive: Failed reading header from `%" PATH_FMT "'", path); \
        return false;
    }

    /* Check if this is an archive by looking for the magic "!<arch>\n"
     * string.  Usually, if this fails, we belch an error and return.  On
     * Darwin however, we may have a fat archive, which contains archives for
     * more than one architecture.  Fat archives start with the magic number
     * 0xcafebabe, always stored big endian.  If we find a fat_header, we scan
     * through the fat_arch structs, searching through for one for our host
     * architecture.  If a matching struct is found, we read the offset
     * of our archive data (nfat_offset) and seek forward nfat_offset bytes
     * from the start of the file.
     *
     * A subtlety is that all of the members of the fat_header and fat_arch
     * structs are stored big endian, so we need to call byte order
     * conversion functions.
     *
     * If we find the appropriate architecture in a fat archive, we gobble
     * its magic "!<arch>\n" string and continue processing just as if
     * we had a single architecture archive.
     */
    if (strncmp(tmp, "!<arch>\n", 8) == 0) {
        *out = StandardArchive;
        return true;
    }
    /* Check if this is a thin archive by looking for the magic string "!<thin>\n"
     *
     * ar thin libraries have the exact same format as normal archives except they
     * have a different magic string and they don't copy the object files into the
     * archive.
     *
     * Instead each header entry points to the location of the object file on disk.
     * This is useful when a library is only created to satisfy a compile time dependency
     * instead of to be distributed. This saves the time required for copying.
     *
     * Thin archives are always flattened. They always only contain simple headers
     * pointing to the object file and so we need not allocate more memory than needed
     * to find the object file.
     *
     */
    else if (strncmp(tmp, "!<thin>\n", 8) == 0) {
        *out = ThinArchive;
        return true;
    }
    else {
        bool success = checkFatArchive(tmp, f, path);
        if (!success) {
            return false;
        }
        *out = FatArchive;
        return true;
    }
}

ll_hs_int loadArchive_ (pathchar *path)
{
    char *image = NULL;
    ll_hs_int retcode = 0;
    int memberIdx = 0;
    FILE *f = NULL;
    size_t thisFileNameSize = (size_t) -1; /* shut up bogus GCC warning */
    int misalignment = 0;

    DEBUG_LOG("start\n");
    DEBUG_LOG("Loading archive `%" PATH_FMT "'\n", path);

    /* Check that we haven't already loaded this archive.
       Ignore requests to load multiple times */
    if (isAlreadyLoaded(path)) {
        LL_IF_DEBUG(linker,
                 LL_DEBUG("ignoring repeated load of %" PATH_FMT "\n", path));
        return 1; /* success */
    }

    char *gnuFileIndex = NULL;
    int gnuFileIndexSize = 0;

    size_t fileNameSize = 32;
    char *fileName = LL_MALLOC(fileNameSize, "loadArchive(fileName)");

    f = pathopen(path, WSTR("rb"));
    if (!f)
        FAIL("loadObj: can't read `%" PATH_FMT "'", path);

    enum ArchiveFormat archive_fmt;
    if (!identifyArchiveFormat(f, path, &archive_fmt)) {
        FAIL("failed to identify archive format of %" PATH_FMT ".", path);
    }
    bool isThin = archive_fmt == ThinArchive;

    DEBUG_LOG("loading archive contents\n");

    while (1) {
        size_t n;
        DEBUG_LOG("reading at %ld\n", ftell(f));
        n = fread ( fileName, 1, 16, f );
        if (n != 16) {
            if (feof(f)) {
                DEBUG_LOG("EOF while reading from '%" PATH_FMT "'\n", path);
                break;
            }
            else {
                FAIL("Failed reading file name from `%" PATH_FMT "'", path);
            }
        }

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
        if (strncmp(fileName, "!<arch>\n", 8) == 0) {
            DEBUG_LOG("found the start of another archive, breaking\n");
            break;
        }
#endif

        char tmp[32];
        n = fread ( tmp, 1, 12, f );
        if (n != 12)
            FAIL("Failed reading mod time from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            FAIL("Failed reading owner from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            FAIL("Failed reading group from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 8, f );
        if (n != 8)
            FAIL("Failed reading mode from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 10, f );
        if (n != 10)
            FAIL("Failed reading size from `%" PATH_FMT "'", path);
        tmp[10] = '\0';
        for (n = 0; isdigit(tmp[n]); n++);
        tmp[n] = '\0';
        size_t memberSize;
        {
            char *end;
            memberSize = strtol(tmp, &end, 10);
            if (tmp == end) {
                FAIL("Failed to decode member size");
            }
        }

        DEBUG_LOG("size of this archive member is %zd\n", memberSize);
        n = fread ( tmp, 1, 2, f );
        if (n != 2)
            FAIL("Failed reading magic from `%" PATH_FMT "'", path);
        if (strncmp(tmp, "\x60\x0A", 2) != 0)
            FAIL("Failed reading magic from `%" PATH_FMT "' at %ld. Got %c%c",
                 path, ftell(f), tmp[0], tmp[1]);

        bool isGnuIndex = false;
        /* Check for BSD-variant large filenames */
        if (0 == strncmp(fileName, "#1/", 3)) {
            size_t n = 0;
            fileName[16] = '\0';
            if (isdigit(fileName[3])) {
                for (n = 4; isdigit(fileName[n]); n++)
                    ;

                fileName[n] = '\0';
                thisFileNameSize = atoi(fileName + 3);
                memberSize -= thisFileNameSize;
                if (thisFileNameSize >= fileNameSize) {
                    /* Double it to avoid potentially continually
                       increasing it by 1 */
                    fileNameSize = thisFileNameSize * 2;
                    fileName = LL_REALLOC(fileName, fileNameSize,
                                               "loadArchive(fileName)");
                }
                n = fread(fileName, 1, thisFileNameSize, f);
                if (n != thisFileNameSize) {
                    LL_LOG_ERROR("Failed reading filename from `%" PATH_FMT "'",
                               path);
                    goto fail;
                }
                fileName[thisFileNameSize] = 0;
                /* On OS X at least, thisFileNameSize is the size of the
                   fileName field, not the length of the fileName
                   itself. */
                thisFileNameSize = strlen(fileName);
            } else {
                LL_LOG_ERROR("BSD-variant filename size not found "
                           "while reading filename from `%" PATH_FMT "'", path);
                goto fail;
            }
        }
        /* Check for GNU file index file */
        else if (0 == strncmp(fileName, "//", 2)) {
            fileName[0] = '\0';
            thisFileNameSize = 0;
            isGnuIndex = true;
        }
        /* Check for a file in the GNU file index */
        else if (fileName[0] == '/') {
            if (!lookupGNUArchiveIndex(gnuFileIndexSize, &fileName,
                     gnuFileIndex, path, &thisFileNameSize, &fileNameSize)) {
                goto fail;
            }
        }
        /* Finally, the case where the filename field actually contains
           the filename */
        else {
            /* GNU ar terminates filenames with a '/', this allowing
               spaces in filenames. So first look to see if there is a
               terminating '/'. */
            for (thisFileNameSize = 0;
                 thisFileNameSize < 16;
                 thisFileNameSize++) {
                if (fileName[thisFileNameSize] == '/') {
                    fileName[thisFileNameSize] = '\0';
                    break;
                }
            }
            /* If we didn't find a '/', then a space terminates the
               filename. Note that if we don't find one, then
               thisFileNameSize ends up as 16, and we already have the
               '\0' at the end. */
            if (thisFileNameSize == 16) {
                for (thisFileNameSize = 0;
                     thisFileNameSize < 16;
                     thisFileNameSize++) {
                    if (fileName[thisFileNameSize] == ' ') {
                        fileName[thisFileNameSize] = '\0';
                        break;
                    }
                }
            }
        }

        DEBUG_LOG("Found member file `%s'\n", fileName);

        bool is_symbol_table = strcmp("", fileName) == 0;
        enum ObjectFileFormat object_fmt = is_symbol_table ? NotObject : identifyObjectFile(f);

#if defined(OBJFORMAT_PEi386)
        /*
        * Note [MSVC import files (ext .lib)]
        * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * MSVC compilers store the object files in
        * the import libraries with extension .dll
        * so on Windows we should look for those too.
        * The PE COFF format doesn't specify any specific file name
        * for sections. So on windows, just try to load it all.
        *
        * Linker members (e.g. filename / are skipped since they are not needed)
        */
        bool isImportLib = thisFileNameSize >= 4 && strncmp(fileName + thisFileNameSize - 4, ".dll", 4) == 0;
#else
        bool isImportLib = false;
#endif // windows

        DEBUG_LOG("\tthisFileNameSize = %d\n", (int)thisFileNameSize);
        DEBUG_LOG("\tisObject = %d\n", object_fmt);

        if ((!is_symbol_table && isThin) || object_fmt != NotObject) {
            DEBUG_LOG("Member is an object file...loading...\n");

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
            if (RTS_LINKER_USE_MMAP)
                image = mmapAnonForLinker(memberSize);
            else {
                /* See loadObj() */
                misalignment = machoGetMisalignment(f);
                image = LL_MALLOC(memberSize + misalignment,
                                        "loadArchive(image)");
                image += misalignment;
            }

#else // not darwin
            image = LL_MALLOC(memberSize, "loadArchive(image)");
#endif
            if (isThin) {
                if (!readThinArchiveMember(n, memberSize, path, fileName, image)) {
                    goto fail;
                }
                // Unlike for regular archives for thin archives we can only identify the object format
                // after having read the file pointed to.
                object_fmt = identifyObjectFile_(image, memberSize);
            }
            else
            {
                size_t n = fread ( image, 1, memberSize, f );
                if (n != memberSize) {
                    FAIL("error whilst reading `%" PATH_FMT "'", path);
                }
            }

            int size = pathprintf(NULL, 0, WSTR("%" PATH_FMT "(#%d:%.*s)"),
                                  path, memberIdx, (int)thisFileNameSize, fileName);
            // I don't understand why this extra +1 is needed here; pathprintf
            // should have given us the correct length but in practice it seems
            // to be one byte short on Win32.
            pathchar *archiveMemberName = LL_MALLOC((size+1+1) * sizeof(pathchar), "loadArchive(file)");
            pathprintf(archiveMemberName, size+1, WSTR("%" PATH_FMT "(#%d:%.*s)"),
                       path, memberIdx, (int)thisFileNameSize, fileName);

            ObjectCode *oc = mkOc(&ll_default_linker, STATIC_OBJECT, path, image, memberSize, false, archiveMemberName,
                                  misalignment);
#if defined(OBJFORMAT_MACHO)
            LL_ASSERT(object_fmt == MachO32 || object_fmt == MachO64);
            ocInit_MachO( oc );
#endif
#if defined(OBJFORMAT_ELF)
            LL_ASSERT(object_fmt == ELF);
            ocInit_ELF( oc );
#endif

            LL_FREE(archiveMemberName);

            if (0 == loadOc(oc)) {
                LL_FREE(fileName);
                fclose(f);
                return 0;
            } else {
                if (ll_host.object_registered != NULL) {
                    ll_host.object_registered(ll_host.ctx, oc); // also adds the object to `objects` list
                }
                oc->next_loaded_object = ll_default_linker.gc.loaded_objects;
                ll_default_linker.gc.loaded_objects = oc;
            }
        }
        else if (isGnuIndex) {
            if (gnuFileIndex != NULL) {
                FAIL("GNU-variant index found, but already have an index, \
while reading filename from `%" PATH_FMT "'", path);
            }
            DEBUG_LOG("Found GNU-variant file index\n");
#if RTS_LINKER_USE_MMAP
            gnuFileIndex = mmapAnonForLinker(memberSize + 1);
#else
            gnuFileIndex = LL_MALLOC(memberSize + 1, "loadArchive(image)");
#endif
            n = fread ( gnuFileIndex, 1, memberSize, f );
            if (n != memberSize) {
                FAIL("error whilst reading `%" PATH_FMT "'", path);
            }
            gnuFileIndex[memberSize] = '/';
            gnuFileIndexSize = memberSize;
        }
        else if (isImportLib) {
#if defined(OBJFORMAT_PEi386)
            if (checkAndLoadImportLibrary(path, fileName, f)) {
                DEBUG_LOG("Member is an import file section... "
                          "Corresponding DLL has been loaded...\n");
            }
            else {
                DEBUG_LOG("Member is not a valid import file section... "
                          "Skipping...\n");
                n = fseek(f, memberSize, SEEK_CUR);
                if (n != 0)
                    FAIL("error whilst seeking by %zd in `%" PATH_FMT "'",
                    memberSize, path);
            }
#endif
        }
        else {
            DEBUG_LOG("`%s' does not appear to be an object file\n",
                      fileName);
            if (!isThin || thisFileNameSize == 0) {
                n = fseek(f, memberSize, SEEK_CUR);
                if (n != 0)
                    FAIL("error whilst seeking by %zd in `%" PATH_FMT "'",
                         memberSize, path);
            }
        }

        /* .ar files are 2-byte aligned */
        if (!(isThin && thisFileNameSize > 0) && memberSize % 2) {
            DEBUG_LOG("trying to read one pad byte\n");
            n = fread ( tmp, 1, 1, f );
            if (n != 1) {
                if (feof(f)) {
                    DEBUG_LOG("found EOF while reading one pad byte\n");
                    break;
                }
                else {
                    FAIL("Failed reading padding from `%" PATH_FMT "'", path);
                }
            }
            DEBUG_LOG("successfully read one pad byte\n");
        }
        memberIdx ++;
        DEBUG_LOG("reached end of archive loading while loop\n");
    }
    retcode = 1;
fail:
    if (f != NULL)
        fclose(f);

    if (fileName != NULL)
        LL_FREE(fileName);
    if (gnuFileIndex != NULL) {
#if RTS_LINKER_USE_MMAP
        munmapForLinker(gnuFileIndex, gnuFileIndexSize + 1, "loadArchive_");
#else
        LL_FREE(gnuFileIndex);
#endif
    }

    DEBUG_LOG("done\n");
    return retcode;
}

ll_hs_int loadArchive (pathchar *path)
{
   ll_mutex_lock(&ll_default_linker.linker_mutex);
   ll_hs_int r = loadArchive_(path);
   ll_mutex_unlock(&ll_default_linker.linker_mutex);
   return r;
}

bool isArchive (pathchar *path)
{
    static const char ARCHIVE_HEADER[] = "!<arch>\n";
    char buffer[10];
    FILE *f = pathopen(path, WSTR("rb"));
    if (f == NULL) {
        return false;
    }

    size_t ret = fread(buffer, 1, sizeof(buffer), f);
    fclose(f);
    if (ret < sizeof(buffer)) {
        return false;
    }
    return strncmp(ARCHIVE_HEADER, buffer, sizeof(ARCHIVE_HEADER)-1) == 0;
}

