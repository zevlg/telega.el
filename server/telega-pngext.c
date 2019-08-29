/*
 * PNG extractor for telega
 */
#define _GNU_SOURCE             /* for memmem() */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include "telega-dat.h"

/*
 * Write PNGDATA to FILENAME
 * Return 0 on success
 */
static int
pngext_write_file(const char* filename, struct telega_dat* pngdata)
{
        int fd = open(filename, O_CREAT | O_WRONLY, 0644);
        if (fd < 0)
                return -1;

        size_t wlen = write(fd, tdat_start(pngdata), tdat_len(pngdata));
        close(fd);

        return wlen != tdat_len(pngdata);
}

/*
 * Extract PNG data from SRC, put it to DST
 * Return 0 on success
 */
static int
pngext_get_png(struct telega_dat* src, struct telega_dat* png)
{
        static char png_begin[] = "\x89" "PNG" "\x0d\x0a\x1a\x0a";
        static char png_end[] = "IEND" "\xae\x42\x60\x82";

        char* start = (char*)memmem(tdat_start(src), tdat_len(src),
                                    png_begin, 8);
        assert(start < tdat_end(src));
        if (start && start > tdat_start(src)) {
                /* Drain the junk befor PNG image data */
                tdat_drain(src, tdat_start(src) - start);
        }
        char* end = (char*)memmem(tdat_start(src), tdat_len(src),
                                  png_end, 8);
        assert(end < tdat_end(src));
        if (end) {
                /* FOUND */
                assert(tdat_len(src) >= end - tdat_start(src) + 8);
                tdat_move(src, png, end - tdat_start(src) + 8);
                return 0;
        }
        return 1;               /* not found */
}

void
pngext_loop(const char* prefix, size_t rdsize)
{
        assert(strlen(prefix) < 500);
        char* png_filename = malloc(strlen(prefix) + 32);
        assert(png_filename != NULL);

        int frame_num = 0;
        struct telega_dat input = TDAT_INIT;
        struct telega_dat png_data = TDAT_INIT;

        ssize_t rlen = 0;
        do {
                tdat_ensure(&input, rdsize);
                rlen = read(0, tdat_end(&input), rdsize);
                if (rlen > 0)
                        input.end += rlen;

                if (!pngext_get_png(&input, &png_data)) {
                        char png_filename[512];
                        snprintf(png_filename, 512, "%s%d.png", prefix, ++frame_num);
                        if (!pngext_write_file(png_filename, &png_data))
                                printf("%d %s\n", frame_num, png_filename);
                        tdat_reset(&png_data);
                }
        } while (rlen > 0);

        tdat_drop(&input);
        tdat_drop(&png_data);
}
