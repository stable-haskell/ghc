#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"

#include "ll_path.h"
#include "ll_failure.h"
#include "ll_error.h"

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
#include <limits.h>
#include "ll_fs.h"

#define ARCHIVE_ERROR(L, STATUS, ...) do {\
   ll_error_set(L, STATUS, "loadArchive: "__VA_ARGS__); \
   LL_LOG_ERROR_FOR(L, "loadArchive: "__VA_ARGS__); \
} while (0)

#define FAIL_STATUS(L, STATUS, ...) do {\
   ARCHIVE_ERROR(L, STATUS, __VA_ARGS__); \
   goto fail;\
} while (0)

#define FAIL(L, ...) FAIL_STATUS(L, LL_E_FORMAT, __VA_ARGS__)
#define FAIL_READ(L, F, ...) \
   FAIL_STATUS(L, ferror(F) ? LL_E_IO : LL_E_FORMAT, __VA_ARGS__)

#define DEBUG_LOG(L, ...) LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "loadArchive: " __VA_ARGS__))

/* Archive numeric fields contain decimal digits followed by padding spaces.
 * Parse within the field, without narrowing or relying on a NUL terminator. */
static bool parseArchiveSize(const char *field, size_t len, uintmax_t *out)
{
    uintmax_t value = 0;
    size_t i = 0;
    while (i < len && field[i] >= '0' && field[i] <= '9') {
        unsigned digit = (unsigned)(field[i] - '0');
        if (value > (UINTMAX_MAX - digit) / 10)
            return false;
        value = value * 10 + digit;
        i++;
    }
    if (i == 0)
        return false;
    for (; i < len; i++) {
        if (field[i] != ' ')
            return false;
    }
    *out = value;
    return true;
}

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
/* Read 4 bytes and convert to host byte order */
static uint32_t read4Bytes(const char buf[static 4])
{
    uint32_t value;
    memcpy(&value, buf, sizeof(value));
    return ntohl(value);
}

static bool loadFatArchive(ll_linker *L, char input[static 8], FILE* f, pathchar* path)
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
    DEBUG_LOG(L, "found a fat archive containing %d architectures\n", nfat_arch);
    char tmp[20];
    nfat_offset = 0;
    for (uint32_t i = 0; i < nfat_arch; i++) {
        /* search for the right arch */
        int n = fread(tmp, 1, 12, f);
        if (n != 12) {
            ARCHIVE_ERROR(L, ferror(f) ? LL_E_IO : LL_E_FORMAT,
                          "Failed reading arch from `%" PATH_FMT "'", path);
            return false;
        }
        cputype = read4Bytes(tmp);
        cpusubtype = read4Bytes(tmp + 4);
        if (cputype == mycputype && cpusubtype == mycpusubtype) {
            DEBUG_LOG(L, "found my archive in a fat archive\n");
            nfat_offset = read4Bytes(tmp + 8);
            break;
        }
    }
    if (nfat_offset == 0) {
        ARCHIVE_ERROR(L, LL_E_UNSUPPORTED, "Fat archive contains %d architectures, "
                   "but none of them are compatible with the host",
                   (int)nfat_arch);
        return false;
    } else {
        /* Seek to the correct architecture */
        int n = fseek(f, nfat_offset, SEEK_SET);
        if (n != 0) {
            ARCHIVE_ERROR(L, LL_E_IO, "Failed to seek to arch in `%" PATH_FMT "'", path);
            return false;
        }

        /* Read the header */
        char tmp[20];
        n = fread(tmp, 1, 8, f);
        if (n != 8) {
            ARCHIVE_ERROR(L, ferror(f) ? LL_E_IO : LL_E_FORMAT,
                          "Failed reading header from `%" PATH_FMT "'", path);
            return false;
        }

        /* Check the magic number */
        if (strncmp(tmp, "!<arch>\n", 8) != 0) {
            ARCHIVE_ERROR(L, LL_E_FORMAT, "couldn't find archive in `%" PATH_FMT "'"
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
    uint16_t magic16 = 0;
    uint32_t magic32 = 0;
    uint64_t magic64 = 0;
    if (sz >= sizeof(magic16))
        memcpy(&magic16, buf, sizeof(magic16));
    if (sz >= sizeof(magic32))
        memcpy(&magic32, buf, sizeof(magic32));
    if (sz >= sizeof(magic64))
        memcpy(&magic64, buf, sizeof(magic64));
    if (sz > 2 && magic16 == 0x8664) {
        return COFFAmd64;
    }
    if (sz > 2 && magic16 == 0x014c) {
        return COFFI386;
    }
    if (sz > 2 && magic16 == 0xaa64) {
        return COFFAArch64;
    }
    if (sz > 4 && memcmp(buf, "\x7f" "ELF", 4) == 0) {
        return ELF;
    }
    if (sz > 4 && magic32 == 0xfeedface) {
        return MachO32;
    }
    if (sz > 4 && magic32 == 0xfeedfacf) {
        return MachO64;
    }
    // BigObj COFF files ...
    if (sz > 8 && magic64 == UINT64_C(0x86640002ffff0000)) {
        return COFFAmd64;
    }
    return NotObject;
}

static bool identifyObjectFile(ll_linker *L, FILE *f, size_t memberSize,
                               pathchar *path, enum ObjectFileFormat *out)
{
    char buf[32];
    size_t len = memberSize < sizeof(buf) ? memberSize : sizeof(buf);
    size_t sz = fread(buf, 1, len, f);
    if (sz != len) {
        ARCHIVE_ERROR(L, ferror(f) ? LL_E_IO : LL_E_FORMAT,
                      "truncated member in `%" PATH_FMT "'", path);
        return false;
    }
    if (fseek(f, -(long)sz, SEEK_CUR) != 0) {
        ARCHIVE_ERROR(L, LL_E_IO, "cannot seek in `%" PATH_FMT "'", path);
        return false;
    }
    *out = identifyObjectFile_(buf, sz);
    return true;
}

static bool readThinArchiveMember(ll_linker *L, size_t memberSize, pathchar* path,
        char* fileName, char* image)
{
    bool has_succeeded = false;
    FILE* member = NULL;
    pathchar *pathCopy, *dirName, *memberPath, *objFileName;
    memberPath = NULL;
    /* Allocate and setup the dirname of the archive.  We'll need
     this to locate the thin member */
    pathCopy = pathdup(L, path); // Convert the char* to a pathchar*
    dirName = pathdir(L, pathCopy);
    /* Append the relative member name to the dirname.  This should be
     be the full path to the actual thin member. */
    objFileName = mkPath(L, fileName);
    size_t dirLen = pathlen(dirName);
    size_t objLen = pathlen(objFileName);
    if (dirLen > INT_MAX - 1 || objLen > (size_t)INT_MAX - 1 - dirLen) {
        ARCHIVE_ERROR(L, LL_E_UNSUPPORTED, "thin member path is too long");
        LL_FREE_FOR(L, objFileName);
        LL_FREE_FOR(L, dirName);
        goto inner_fail;
    }
    size_t memberLen = dirLen + objLen + 1;
    if (memberLen > SIZE_MAX / pathsize) {
        ARCHIVE_ERROR(L, LL_E_UNSUPPORTED, "thin member path is too long");
        LL_FREE_FOR(L, objFileName);
        LL_FREE_FOR(L, dirName);
        goto inner_fail;
    }
    memberPath = LL_MALLOC_FOR(L, pathsize * memberLen, "loadArchive(file)");
    int written = pathprintf(memberPath, memberLen, WSTR("%" PATH_FMT "%" PATH_FMT),
                            dirName, objFileName);
    LL_FREE_FOR(L, objFileName);
    LL_FREE_FOR(L, dirName);
    if (written < 0 || (size_t)written >= memberLen) {
        ARCHIVE_ERROR(L, LL_E_FORMAT, "cannot encode thin member path");
        goto inner_fail;
    }
    member = pathopen(memberPath, WSTR("rb"));
    if (!member) {
        ARCHIVE_ERROR(L, LL_E_IO, "can't read thin archive member `%" PATH_FMT "'",
                   memberPath);
        goto inner_fail;
    }
    size_t n = fread(image, 1, memberSize, member);
    if (n != memberSize) {
        ARCHIVE_ERROR(L, ferror(member) ? LL_E_IO : LL_E_FORMAT,
                     "error whilst reading `%s'",
                   fileName);
        goto inner_fail;
    }
    has_succeeded = true;

inner_fail:
    if (member != NULL)
        fclose(member);
    LL_FREE_FOR(L, memberPath);
    LL_FREE_FOR(L, pathCopy);
    return has_succeeded;
}

static bool checkFatArchive(ll_linker *L, char magic[static 4], FILE* f, pathchar* path)
{
    bool success = false;
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
    /* Not a standard archive, look for a fat archive magic number: */
    if (read4Bytes(magic) == FAT_MAGIC)
        success = loadFatArchive(L, magic, f, path);
    else
        ARCHIVE_ERROR(L, LL_E_FORMAT, "Neither an archive, nor a fat archive: "
                   "`%" PATH_FMT "'", path);
#else
    (void)magic;
    (void)f;
    ARCHIVE_ERROR(L, LL_E_FORMAT, "Not an archive: `%" PATH_FMT "'", path);
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
lookupGNUArchiveIndex(ll_linker *L, size_t gnuFileIndexSize, char **fileName_,
    char* gnuFileIndex, pathchar* path, size_t* thisFileNameSize,
    size_t* fileNameSize)
{
    char *fileName = *fileName_;
    if (fileName[1] >= '0' && fileName[1] <= '9') {
        if (gnuFileIndex == NULL) {
            ARCHIVE_ERROR(L, LL_E_FORMAT, "GNU-variant filename "
                    "without an index while reading from `%" PATH_FMT "'",
                    path);
            return false;
        }

        uintmax_t offset;
        if (!parseArchiveSize(fileName + 1, 15, &offset)) {
            ARCHIVE_ERROR(L, LL_E_FORMAT, "invalid GNU-variant filename offset "
                          "in `%" PATH_FMT "'", path);
            return false;
        }
        if (offset >= gnuFileIndexSize) {
            ARCHIVE_ERROR(L, LL_E_FORMAT, "GNU-variant filename "
                    "offset %ju out of range [0..%zu) "
                    "while reading filename from `%" PATH_FMT "'",
                    offset, gnuFileIndexSize, path);
            return false;
        }
        size_t n = (size_t)offset;

        // Check that the previous entry ends with the expected
        // end-of-string delimiter.
#if defined(mingw32_HOST_OS)
#define IS_SYMBOL_DELIMITER(STR) (STR =='\n' || STR == '\0')
#else
#define IS_SYMBOL_DELIMITER(STR) (STR =='\n')
#endif
        if (n != 0 && !IS_SYMBOL_DELIMITER(gnuFileIndex[n - 1])) {
            ARCHIVE_ERROR(L, LL_E_FORMAT, "GNU-variant filename offset "
                    "%zu invalid (range [0..%zu)) while reading "
                    "filename from `%" PATH_FMT "'",
                    n, gnuFileIndexSize, path);
            return false;
        }

        size_t i;
        for (i = n; i < gnuFileIndexSize && !IS_SYMBOL_DELIMITER(gnuFileIndex[i]); i++)
            ;

        if (i == gnuFileIndexSize || i == n) {
            ARCHIVE_ERROR(L, LL_E_FORMAT, "unterminated or empty GNU-variant "
                          "filename in `%" PATH_FMT "'", path);
            return false;
        }

        size_t FileNameSize = i - n;
        if (FileNameSize >= *fileNameSize) {
            /* Double it to avoid potentially continually
             increasing it by 1 */
            *fileNameSize = FileNameSize * 2;
            *fileName_ = fileName = LL_REALLOC_FOR(L, fileName, *fileNameSize,
                    "loadArchive(fileName)");
        }
        memcpy(fileName, gnuFileIndex + n, FileNameSize);


       /* llvm-ar terminates string table entries with `/\n`. */
       if (fileName[FileNameSize-1] == '/') {
           FileNameSize--;
       }

        if (FileNameSize == 0) {
            ARCHIVE_ERROR(L, LL_E_FORMAT, "empty GNU-variant filename in `%" PATH_FMT "'", path);
            return false;
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
        ARCHIVE_ERROR(L, LL_E_FORMAT, "invalid GNU-variant filename `%.16s' "
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

static bool identifyArchiveFormat(ll_linker *L, FILE *f, pathchar *path, enum ArchiveFormat *out)
{
    char tmp[8];
    size_t n = fread(tmp, 1, 8, f);
    if (n != 8) {
        ARCHIVE_ERROR(L, ferror(f) ? LL_E_IO : LL_E_FORMAT,
                      "Failed reading header from `%" PATH_FMT "'", path);
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
        bool success = checkFatArchive(L, tmp, f, path);
        if (!success) {
            return false;
        }
        *out = FatArchive;
        return true;
    }
}

/* Seeking alone can succeed past EOF. Touch the last declared payload byte
 * before continuing, including for skipped symbol tables/nonobject members. */
static bool skipArchiveMember(ll_linker *L, FILE *f, pathchar *path, size_t size)
{
    if (size == 0)
        return true;
    if (fseek(f, (long)(size - 1), SEEK_CUR) != 0) {
        ARCHIVE_ERROR(L, LL_E_IO, "cannot seek over member in `%" PATH_FMT "'", path);
        return false;
    }
    if (fgetc(f) == EOF) {
        ARCHIVE_ERROR(L, ferror(f) ? LL_E_IO : LL_E_FORMAT,
                      "truncated member in `%" PATH_FMT "'", path);
        return false;
    }
    return true;
}

ll_hs_int loadArchive_ (ll_linker *L, pathchar *path)
{
    char *image = NULL;
    char *imageBase = NULL;
    size_t imageSize = 0;
    bool imageMapped = false;
    ll_hs_int retcode = 0;
    int memberIdx = 0;
    FILE *f = NULL;
    size_t thisFileNameSize = (size_t) -1; /* shut up bogus GCC warning */
    int misalignment = 0;

    if (path == NULL) {
        ll_error_set(L, LL_E_STATE, "loadArchive: archive path is NULL");
        return 0;
    }

    if (!ll_failure_path_usable(L, path)) return 0;
    DEBUG_LOG(L, "start\n");
    DEBUG_LOG(L, "Loading archive `%" PATH_FMT "'\n", path);

    /* Check that we haven't already loaded this archive.
       Ignore requests to load multiple times */
    if (isAlreadyLoaded(L, path)) {
        LL_IF_DEBUG_FOR(L, linker,
                 LL_DEBUG_FOR(L, "ignoring repeated load of %" PATH_FMT "\n", path));
        return 1; /* success */
    }

    char *gnuFileIndex = NULL;
    size_t gnuFileIndexSize = 0;

    size_t fileNameSize = 32;
    char *fileName = LL_MALLOC_FOR(L, fileNameSize, "loadArchive(fileName)");

    f = pathopen(path, WSTR("rb"));
    if (!f)
        FAIL_STATUS(L, LL_E_IO, "can't read `%" PATH_FMT "'", path);

    enum ArchiveFormat archive_fmt;
    if (!identifyArchiveFormat(L, f, path, &archive_fmt)) {
        goto fail;
    }
    bool isThin = archive_fmt == ThinArchive;

    DEBUG_LOG(L, "loading archive contents\n");

    while (1) {
        size_t n;
        DEBUG_LOG(L, "reading at %ld\n", ftell(f));
        n = fread ( fileName, 1, 16, f );
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
        /* A following FAT slice can consist only of its eight-byte archive
         * magic. Recognize the slice boundary before rejecting a short name. */
        if (archive_fmt == FatArchive && n >= 8 &&
            memcmp(fileName, "!<arch>\n", 8) == 0) {
            DEBUG_LOG(L, "found the start of another archive, breaking\n");
            break;
        }
#endif
        if (n != 16) {
            if (n == 0 && feof(f) && !ferror(f)) {
                DEBUG_LOG(L, "EOF while reading from '%" PATH_FMT "'\n", path);
                break;
            }
            else {
                FAIL_READ(L, f, "Failed reading file name from `%" PATH_FMT "'", path);
            }
        }
        fileName[16] = '\0';

        char tmp[32];
        n = fread ( tmp, 1, 12, f );
        if (n != 12)
            FAIL_READ(L, f, "Failed reading mod time from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            FAIL_READ(L, f, "Failed reading owner from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            FAIL_READ(L, f, "Failed reading group from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 8, f );
        if (n != 8)
            FAIL_READ(L, f, "Failed reading mode from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 10, f );
        if (n != 10)
            FAIL_READ(L, f, "Failed reading size from `%" PATH_FMT "'", path);
        uintmax_t memberSizeField;
        if (!parseArchiveSize(tmp, 10, &memberSizeField))
            FAIL(L, "Failed to decode member size in `%" PATH_FMT "'", path);
        if (memberSizeField > INT_MAX)
            FAIL_STATUS(L, LL_E_UNSUPPORTED,
                        "member is too large in `%" PATH_FMT "'", path);
        size_t memberSize = (size_t)memberSizeField;
        size_t storedMemberSize = memberSize;

        DEBUG_LOG(L, "size of this archive member is %zd\n", memberSize);
        n = fread ( tmp, 1, 2, f );
        if (n != 2)
            FAIL_READ(L, f, "Failed reading magic from `%" PATH_FMT "'", path);
        if (strncmp(tmp, "\x60\x0A", 2) != 0)
            FAIL(L, "Failed reading magic from `%" PATH_FMT "' at %ld. Got %c%c",
                 path, ftell(f), tmp[0], tmp[1]);

        bool isGnuIndex = false;
        /* Check for BSD-variant large filenames */
        if (0 == strncmp(fileName, "#1/", 3)) {
            uintmax_t nameSize;
            if (parseArchiveSize(fileName + 3, 13, &nameSize)
                && nameSize > 0 && nameSize <= memberSize) {
                thisFileNameSize = (size_t)nameSize;
                memberSize -= thisFileNameSize;
                if (thisFileNameSize >= fileNameSize) {
                    /* Double it to avoid potentially continually
                       increasing it by 1 */
                    fileNameSize = thisFileNameSize * 2;
                    fileName = LL_REALLOC_FOR(L, fileName, fileNameSize,
                                               "loadArchive(fileName)");
                }
                n = fread(fileName, 1, thisFileNameSize, f);
                if (n != thisFileNameSize) {
                    FAIL_READ(L, f, "Failed reading filename from `%" PATH_FMT "'", path);
                }
                fileName[thisFileNameSize] = 0;
                /* On OS X at least, thisFileNameSize is the size of the
                   fileName field, not the length of the fileName
                   itself. */
                thisFileNameSize = strlen(fileName);
            } else {
                FAIL(L, "invalid BSD-variant filename size in `%" PATH_FMT "'", path);
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
            if (!lookupGNUArchiveIndex(L, gnuFileIndexSize, &fileName,
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

        DEBUG_LOG(L, "Found member file `%s'\n", fileName);

        bool is_symbol_table = strcmp("", fileName) == 0;
        enum ObjectFileFormat object_fmt = NotObject;
        if (!is_symbol_table && !isThin &&
            !identifyObjectFile(L, f, memberSize, path, &object_fmt))
            goto fail;

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

        DEBUG_LOG(L, "\tthisFileNameSize = %d\n", (int)thisFileNameSize);
        DEBUG_LOG(L, "\tisObject = %d\n", object_fmt);

        if ((!is_symbol_table && isThin) || object_fmt != NotObject) {
            DEBUG_LOG(L, "Member is an object file...loading...\n");
            if (memberSize == 0)
                FAIL(L, "empty object member in `%" PATH_FMT "'", path);
            imageSize = memberSize;

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
            if (RTS_LINKER_USE_MMAP) {
                image = ll_mmap_anon_for_linker(L, memberSize);
                imageMapped = true;
            }
            else {
                /* See loadObj() */
                misalignment = machoGetMisalignment(L, f);
                if (misalignment < 0)
                    goto fail;
                imageBase = LL_MALLOC_FOR(L, memberSize + misalignment,
                                         "loadArchive(image)");
                image = imageBase + misalignment;
            }

#else // not darwin
            image = LL_MALLOC_FOR(L, memberSize, "loadArchive(image)");
#endif
            if (image == NULL)
                FAIL_STATUS(L, LL_E_NOMEM, "cannot allocate member image in `%" PATH_FMT "'", path);
            if (imageBase == NULL)
                imageBase = image;
            if (isThin) {
                if (!readThinArchiveMember(L, memberSize, path, fileName, image)) {
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
                    FAIL_READ(L, f, "error whilst reading `%" PATH_FMT "'", path);
                }
            }

            int size = pathprintf(NULL, 0, WSTR("%" PATH_FMT "(#%d:%.*s)"),
                                  path, memberIdx, (int)thisFileNameSize, fileName);
            if (size < 0 || size > INT_MAX - 2)
                FAIL(L, "cannot encode member name in `%" PATH_FMT "'", path);
            // I don't understand why this extra +1 is needed here; pathprintf
            // should have given us the correct length but in practice it seems
            // to be one byte short on Win32.
            pathchar *archiveMemberName = LL_MALLOC_FOR(L, (size+1+1) * sizeof(pathchar), "loadArchive(file)");
            pathprintf(archiveMemberName, size+1, WSTR("%" PATH_FMT "(#%d:%.*s)"),
                       path, memberIdx, (int)thisFileNameSize, fileName);

            ObjectCode *oc = mkOc(L, STATIC_OBJECT, path, image, memberSize, imageMapped, archiveMemberName,
                                  misalignment);
            /* mkOc owns the image from here. Previously registered members
             * retain their existing ownership when a later member fails. */
            image = NULL;
            imageBase = NULL;
            imageMapped = false;
            LL_FREE_FOR(L, archiveMemberName);

            if (!ll_init_object(oc) || !loadOc(oc)) {
                ll_discard_object(oc);
                goto fail;
            } else {
                if (L->services.host.object_registered != NULL) {
                    L->services.host.object_registered(L->services.host.ctx, oc); // also adds the object to `objects` list
                }
                oc->next_loaded_object = L->gc.loaded_objects;
                L->gc.loaded_objects = oc;
            }
        }
        else if (isGnuIndex) {
            if (gnuFileIndex != NULL) {
                FAIL(L, "GNU-variant index found, but already have an index, \
while reading filename from `%" PATH_FMT "'", path);
            }
            DEBUG_LOG(L, "Found GNU-variant file index\n");
#if RTS_LINKER_USE_MMAP
            gnuFileIndex = ll_mmap_anon_for_linker(L, memberSize + 1);
#else
            gnuFileIndex = LL_MALLOC_FOR(L, memberSize + 1, "loadArchive(image)");
#endif
            if (gnuFileIndex == NULL)
                FAIL_STATUS(L, LL_E_NOMEM, "cannot allocate GNU filename index in `%" PATH_FMT "'", path);
            gnuFileIndexSize = memberSize;
            n = fread ( gnuFileIndex, 1, memberSize, f );
            if (n != memberSize) {
                FAIL_READ(L, f, "error whilst reading `%" PATH_FMT "'", path);
            }
            gnuFileIndex[memberSize] = '/';
        }
        else if (isImportLib) {
#if defined(OBJFORMAT_PEi386)
            if (checkAndLoadImportLibrary(L, path, fileName, f)) {
                DEBUG_LOG(L, "Member is an import file section... "
                          "Corresponding DLL has been loaded...\n");
            }
            else {
                DEBUG_LOG(L, "Member is not a valid import file section... "
                          "Skipping...\n");
                if (!skipArchiveMember(L, f, path, memberSize))
                    goto fail;
            }
#endif
        }
        else {
            DEBUG_LOG(L, "`%s' does not appear to be an object file\n",
                      fileName);
            if (!isThin || thisFileNameSize == 0) {
                if (!skipArchiveMember(L, f, path, memberSize))
                    goto fail;
            }
        }

        /* .ar files are 2-byte aligned */
        if (!(isThin && thisFileNameSize > 0) && storedMemberSize % 2) {
            DEBUG_LOG(L, "trying to read one pad byte\n");
            n = fread ( tmp, 1, 1, f );
            if (n != 1) {
                FAIL_READ(L, f, "Failed reading padding from `%" PATH_FMT "'", path);
            }
            DEBUG_LOG(L, "successfully read one pad byte\n");
        }
        memberIdx ++;
        DEBUG_LOG(L, "reached end of archive loading while loop\n");
    }
    retcode = 1;
fail:
    if (imageBase != NULL) {
        if (imageMapped)
            ll_munmap_for_linker(L, imageBase, imageSize, "loadArchive_");
        else
            LL_FREE_FOR(L, imageBase);
    }
    if (f != NULL)
        fclose(f);

    if (fileName != NULL)
        LL_FREE_FOR(L, fileName);
    if (gnuFileIndex != NULL) {
#if RTS_LINKER_USE_MMAP
        ll_munmap_for_linker(L, gnuFileIndex, gnuFileIndexSize + 1, "loadArchive_");
#else
        LL_FREE_FOR(L, gnuFileIndex);
#endif
    }

    DEBUG_LOG(L, "done\n");
    return retcode;
}

ll_hs_int loadArchive (pathchar *path)
{
   ll_mutex_lock(&ll_default_linker.linker_mutex);
   ll_error_clear(&ll_default_linker);
   ll_hs_int r = loadArchive_(&ll_default_linker, path);
   ll_mutex_unlock(&ll_default_linker.linker_mutex);
   return r;
}

bool isArchive (pathchar *path)
{
    static const char ARCHIVE_HEADER[] = "!<arch>\n";
    static const char THIN_ARCHIVE_HEADER[] = "!<thin>\n";
    char buffer[8];
    FILE *f = pathopen(path, WSTR("rb"));
    if (f == NULL) {
        return false;
    }

    size_t ret = fread(buffer, 1, sizeof(buffer), f);
    fclose(f);
    if (ret < sizeof(buffer)) {
        return false;
    }
    return memcmp(ARCHIVE_HEADER, buffer, sizeof(buffer)) == 0 ||
           memcmp(THIN_ARCHIVE_HEADER, buffer, sizeof(buffer)) == 0;
}

