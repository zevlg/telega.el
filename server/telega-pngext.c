/*
 * PNG extractor for telega
 */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE             /* for memmem() */
#endif /* _GNU_SOURCE */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>

#include "telega-dat.h"

int cmd_pid;                    /* PID of external command */

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
                /* Drain the junk before PNG image data */
                tdat_drain(src, tdat_start(src) - start);
        }
        char* end = (char*)memmem(tdat_start(src), tdat_len(src),
                                  png_end, 8);
        assert(end < tdat_end(src));
        if (end) {
                /* FOUND */
                assert(tdat_len(src) >= (size_t)(end - tdat_start(src) + 8));
                tdat_move(src, png, end - tdat_start(src) + 8);
                return 0;
        }
        return 1;               /* not found */
}

void
pngext_usage(char* prog)
{
        printf("usage: %s -E PREFIX [-R RDSIZE] -- CMD [ARGS]\n", prog);
        printf("Captures output from external command CMD and extracts\n"
               "png images from there, writing them to temporary location\n"
               "with PREFIX.\n");
        printf("Used to animate gifs, play voice notes.\n");
        printf("Emacs is extremely bad at processing huge outputs from external commands.\n");
        exit(0);
}

static void
pngext_signal_bypass(int sig)
{
        kill(cmd_pid, sig);
}

void
pngext_loop(const char* prefix, size_t rdsize)
{
        assert(prefix != NULL);
        assert(strlen(prefix) < 500);

        int frame_num = 0;
        struct telega_dat input = TDAT_INIT;
        struct telega_dat png_data = TDAT_INIT;

        /* Non-blocking read from stdin */
        if (fcntl(0, F_SETFL, fcntl(0, F_GETFL) | O_NONBLOCK)) {
                perror("fcntl()");
                return;
        }

        fd_set rfds;
        FD_ZERO(&rfds);
        FD_SET(0, &rfds);
        while (1) {
                tdat_ensure(&input, rdsize);
                int err = select(1, &rfds, NULL, NULL, NULL);
                if (err < 0) {
                        if (errno == EINTR)
                                continue;
                        perror("select()");
                        break;
                        /* NOT REACHED */
                }
                assert(err != 0); /* no timeouts */

                ssize_t rlen = read(0, tdat_end(&input), rdsize);
                if (rlen > 0)
                        input.end += rlen;
                else if (rlen == 0)
                        break;  /* DONE */
                else if (errno == EWOULDBLOCK || errno == EINTR)
                        continue;
                else {
                        perror("read()");
                        break;
                        /* NOT REACHED */
                }

                if (!pngext_get_png(&input, &png_data)) {
                        char png_filename[512];
                        snprintf(png_filename, 512, "%s%d.png", prefix, ++frame_num);
                        if (!pngext_write_file(png_filename, &png_data))
                                printf("%d %s\n", frame_num, png_filename);
                        tdat_reset(&png_data);
                }
        }

        tdat_drop(&input);
        tdat_drop(&png_data);
}

void
pngext_main(int ac, char** av)
{
        char* prefix = NULL;
        int rdsize = 20 * 1024;

        optind = 1;             /* rescan ac/av */
        int ch;
        while ((ch = getopt(ac, av, "E:R:")) != -1) {
                switch (ch) {
                case 'E':
                        prefix = optarg;
                        break;
                case 'R':
                        rdsize = atoi(optarg);
                        break;
                case '-':
                        break;
                default:
                        /* IGNORE */
                        break;
                }
        }

        if (optind >= ac) {
                pngext_usage(av[0]);
                /* NOT REACHED */
        }

        int pipe_fds[2];
        int err = pipe(pipe_fds);
        assert(!err);

        cmd_pid = fork();
        if (cmd_pid == 0) {
                /* child */
                err = close(pipe_fds[0]);
                assert(!err);
                err = dup2(pipe_fds[1], STDOUT_FILENO);
                assert(err >= 0);

                err = execv(av[optind], &av[optind]);
                exit(err);
                /* NOT REACHED */
        } else {
                err = close(pipe_fds[1]);
                assert(!err);
                err = dup2(pipe_fds[0], STDIN_FILENO);
                assert(err >= 0);

                signal(SIGSTOP, pngext_signal_bypass);
                signal(SIGCONT, pngext_signal_bypass);

                pngext_loop(prefix, rdsize);
        }
}
