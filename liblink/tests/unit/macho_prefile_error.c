#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/MachO.c"

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if !defined(darwin_HOST_OS) && !defined(ios_HOST_OS)
#error "macho_prefile_error is a Darwin-only unit"
#endif

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "macho_prefile_error:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

typedef struct {
    unsigned logs;
    unsigned fatals;
} Host;

static Host owner_host;
static Host default_host;
static ll_linker owner;
static unsigned failures;
static char default_message[] = "default-owner-sentinel";

static void host_log(void *ctx, int kind, const char *format, va_list args)
{
    Host *host = ctx;
    char message[512];
    int written = vsnprintf(message, sizeof(message), format, args);
    CHECK(written >= 0 && (size_t)written < sizeof(message),
          "host log formatting failed or truncated");
    (void)kind;
    ++host->logs;
}

static _Noreturn void host_fatal(void *ctx, const char *format, va_list args)
{
    Host *host = ctx;
    char message[512];
    (void)vsnprintf(message, sizeof(message), format, args);
    ++host->fatals;
    (void)fprintf(stderr, "unexpected linker fatal: %s\n", message);
    _Exit(98);
}

static ll_compat_host_ops host_ops(Host *host)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = host;
    ops.log = host_log;
    ops.fatal = host_fatal;
    return ops;
}

static void reset_owner_error(void)
{
    ll_error_clear(&owner);
}

static FILE *header_stream(uint32_t magic, uint32_t command_bytes,
                           long start_offset)
{
    FILE *stream = tmpfile();
    unsigned char prefix[32];
    MachOHeader header;
    if (stream == NULL) return NULL;
    memset(prefix, 0x6D, sizeof(prefix));
    memset(&header, 0, sizeof(header));
    header.magic = magic;
    header.sizeofcmds = command_bytes;
    if (fwrite(prefix, 1, (size_t)start_offset, stream) != (size_t)start_offset ||
        fwrite(&header, sizeof(header), 1, stream) != 1 || fflush(stream) != 0 ||
        fseek(stream, start_offset, SEEK_SET) != 0) {
        (void)fclose(stream);
        return NULL;
    }
    return stream;
}

static void test_valid_padding_residues(void)
{
    static const struct {
        uint32_t command_bytes;
        int expected_padding;
    } cases[] = {
        {0, 0},
        {4, 12},
        {8, 8},
        {12, 4},
    };
    const long start_offset = 11;
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        FILE *stream = header_stream(MH_MAGIC_64, cases[i].command_bytes,
                                     start_offset);
        CHECK(stream != NULL, "could not create valid Mach-O header stream");
        if (stream == NULL) continue;
        reset_owner_error();
        int padding = machoGetMisalignment(&owner, stream);
        CHECK(padding == cases[i].expected_padding,
              "valid header returned incorrect alignment padding");
        CHECK(owner.error_status == LL_OK,
              "valid header changed owner error status");
        CHECK(ftell(stream) == start_offset,
              "valid header did not restore the nonzero stream position");
        CHECK(fclose(stream) == 0, "could not close valid header stream");
    }
}

static void check_format_failure(FILE *stream, const char *case_name)
{
    CHECK(stream != NULL, "could not create malformed Mach-O stream");
    if (stream == NULL) return;
    reset_owner_error();
    CHECK(machoGetMisalignment(&owner, stream) == -1,
          "malformed Mach-O header did not return the error sentinel");
    CHECK(owner.error_status == LL_E_FORMAT,
          "malformed Mach-O header did not report LL_E_FORMAT");
    CHECK(owner.error_message[0] != '\0',
          "malformed Mach-O header did not record a diagnostic");
    CHECK(fclose(stream) == 0, case_name);
}

static void test_bad_magic(void)
{
    const long start_offset = 7;
    FILE *stream = header_stream(0, 0, start_offset);
    CHECK(stream != NULL, "could not create bad-magic stream");
    if (stream == NULL) return;
    reset_owner_error();
    CHECK(machoGetMisalignment(&owner, stream) == -1,
          "bad magic did not return the error sentinel");
    CHECK(owner.error_status == LL_E_FORMAT,
          "bad magic did not report LL_E_FORMAT");
    CHECK(ftell(stream) == start_offset,
          "bad-magic path did not restore the nonzero stream position");
    CHECK(fclose(stream) == 0, "could not close bad-magic stream");
}

static void test_empty_and_short_headers(void)
{
    FILE *empty = tmpfile();
    CHECK(empty != NULL, "could not create empty input stream");
    if (empty != NULL) check_format_failure(empty, "could not close empty stream");

    FILE *short_stream = tmpfile();
    CHECK(short_stream != NULL, "could not create short-header stream");
    if (short_stream != NULL) {
        unsigned char bytes[sizeof(MachOHeader) - 1];
        memset(bytes, 0, sizeof(bytes));
        CHECK(fwrite(bytes, 1, sizeof(bytes), short_stream) == sizeof(bytes),
              "could not write short-header fixture");
        CHECK(fflush(short_stream) == 0 && fseek(short_stream, 0, SEEK_SET) == 0,
              "could not rewind short-header fixture");
        check_format_failure(short_stream, "could not close short-header stream");
    }
}

static void test_read_error(void)
{
    FILE *stream = tmpfile();
    CHECK(stream != NULL, "could not create read-error stream");
    if (stream == NULL) return;
    CHECK(setvbuf(stream, NULL, _IONBF, 0) == 0,
          "could not make read-error stream unbuffered");
    int fd = fileno(stream);
    CHECK(fd >= 0 && close(fd) == 0,
          "could not close read-error stream descriptor");
    reset_owner_error();
    CHECK(machoGetMisalignment(&owner, stream) == -1,
          "failed read did not return the error sentinel");
    CHECK(owner.error_status == LL_E_IO,
          "ferror after failed read did not report LL_E_IO");
    CHECK(fclose(stream) != 0,
          "closed descriptor did not produce the expected stream close error");
}

static void test_nonseekable_stream(void)
{
    int fds[2];
    MachOHeader header;
    int pipe_result = pipe(fds);
    CHECK(pipe_result == 0, "could not create seek-failure pipe");
    if (pipe_result != 0) return;
    memset(&header, 0, sizeof(header));
    header.magic = MH_MAGIC_64;
    header.sizeofcmds = 16;
    ssize_t written = write(fds[1], &header, sizeof(header));
    CHECK(written == (ssize_t)sizeof(header),
          "could not write pipe Mach-O header");
    int close_writer_result = close(fds[1]);
    CHECK(close_writer_result == 0, "could not close pipe writer");
    if (written != (ssize_t)sizeof(header) || close_writer_result != 0) {
        (void)close(fds[0]);
        return;
    }
    FILE *stream = fdopen(fds[0], "rb");
    CHECK(stream != NULL, "could not create pipe stream");
    if (stream == NULL) {
        (void)close(fds[0]);
        return;
    }
    CHECK(setvbuf(stream, NULL, _IONBF, 0) == 0,
          "could not make pipe stream unbuffered");
    reset_owner_error();
    CHECK(machoGetMisalignment(&owner, stream) == -1,
          "nonseekable input did not return the error sentinel");
    CHECK(owner.error_status == LL_E_IO,
          "failed rewind did not report LL_E_IO");
    CHECK(fclose(stream) == 0, "could not close pipe stream");
}

int main(void)
{
    ll_compat_host_ops default_ops = host_ops(&default_host);
    ll_compat_host_ops owner_ops = host_ops(&owner_host);
    ll_install_host(&default_ops, NULL);
    ll_install_host_for(&owner, &owner_ops, NULL);
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_message, sizeof(default_message));
    ll_init_linker(&owner, 0);

    test_valid_padding_residues();
    test_bad_magic();
    test_empty_and_short_headers();
    test_read_error();
    test_nonseekable_stream();
    ll_exit_linker(&owner);

    CHECK(owner_host.logs > 0, "negative paths did not log through owner services");
    CHECK(owner_host.fatals == 0, "unexpected fatal callback on recoverable input");
    CHECK(default_host.logs == 0, "owner errors were logged through default services");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND &&
              memcmp(ll_default_linker.error_message, default_message,
                     sizeof(default_message)) == 0,
          "owner error handling modified default linker status");
    if (failures != 0) return 1;
    (void)puts("macho_prefile_error: PASS");
    return 0;
}
