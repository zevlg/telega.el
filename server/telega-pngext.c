/*
 * PNG extractor for telega
 */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE             /* for memmem() */
#endif /* _GNU_SOURCE */
#include <sys/time.h>
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

int cmd_pid;              /* PID of external command */

/*
 * FPS ratio
 *  - fps_numerator=0, to schedule frames as fast as they arrive
 */
long fps_numerator = 0;
long fps_denominator = 1;

/*
 * Time of playback start, used to calculate timeout to display a frame
 * Recalculade on pause/resume
 */
struct timeval start_time = {0, 0};

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
        if (start == NULL)
                return -1;      /* need more data */

        assert(start < tdat_end(src));
        if (start > tdat_start(src)) {
                /* Drain the junk before PNG image data */
                tdat_drain(src, tdat_start(src) - start);
        }
        char* end = (char*)memmem(tdat_start(src), tdat_len(src),
                                  png_end, 8);
        if (end) {
                /* FOUND */
                assert(end < tdat_end(src));
                assert(tdat_len(src) >= (size_t)(end - tdat_start(src) + 8));
                tdat_move(src, png, end - tdat_start(src) + 8);
                return 0;
        }
        return 1;               /* not found */
}

void
pngext_usage(char* prog)
{
        printf("usage: %s -E PREFIX [-f FPS] [-R RDSIZE] -- CMD [ARGS]\n", prog);
        printf("Captures output from external command CMD and extracts\n"
               "png images from there, writing them to temporary location\n"
               "with PREFIX.\n");
        printf("Used to animate gifs, play voice notes.\n");
        printf("Emacs is extremely bad at processing huge outputs from external commands.\n");
        printf("\t-R RDSIZE    Default is 20K\n");
        printf("\t-f FPS       Extract images in real time fashion using FPS as frame rate\n");
        printf("\t             FPS is specified in form NUMERATOR[/DENOMINATOR]\n");
        printf("\t             for example 30000/1001 for 29.97fps\n");
        printf("\t             if DENOMINATOR is not specified, 1 is used\n");
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
        bool input_done = false; /* true, when all data has been read */
        struct telega_dat input = TDAT_INIT;
        struct telega_dat png_data = TDAT_INIT;
        struct timeval frame_timeout = {0, 0};
        struct timeval* stimeout = NULL;

        /* Non-blocking read from stdin */
        if (fcntl(0, F_SETFL, fcntl(0, F_GETFL) | O_NONBLOCK)) {
                perror("fcntl()");
                return;
        }

        if (gettimeofday(&start_time, NULL)) {
                perror("gettimeofday()");
                return;
        }

        fd_set rfds;
        FD_ZERO(&rfds);
        /*
         * - We are done if there is no input and no pending frame to
         *   display
         * - For the very first frame we always need input, therefore
         *   stimeout is NULL initially
         */
        while (!input_done || stimeout) {
                if (fps_numerator > 0) {
                        /* Calculate timeout for next frame */
                        struct timeval frame_time, now_time;
                        long frame_usec = (1000000 * frame_num
                                           * fps_denominator) / fps_numerator;
                        frame_time.tv_sec = frame_usec / 1000000;
                        frame_time.tv_usec = (frame_usec
                                              - (frame_time.tv_sec * 1000000));

                        timeradd(&start_time, &frame_time, &frame_time);

                        if (gettimeofday(&now_time, NULL)) {
                                perror("gettimeofday()");
                                return;
                        }
                        if (timercmp(&frame_time, &now_time, >)) {
                                timersub(&frame_time, &now_time, &frame_timeout);
                        } else {
                                frame_timeout.tv_sec = 0;
                                frame_timeout.tv_usec = 0;
                        }
                }

                if (!input_done) {
                        FD_SET(0, &rfds);
                } else {
                        FD_ZERO(&rfds);
                        assert(stimeout);
                }

                int err = select(1, &rfds, NULL, NULL, stimeout);
                if (err < 0) {
                        if (errno == EINTR)
                                continue;
                        perror("select()");
                        break;
                        /* NOT REACHED */
                } else if (err == 0) {
                        assert(stimeout != NULL);

                        /* Timeout, display next frame */
                        if (!pngext_get_png(&input, &png_data)) {
                                char png_filename[512];
                                snprintf(png_filename, 512, "%s%d.png", prefix, ++frame_num);
                                if (!pngext_write_file(png_filename, &png_data))
                                        printf("%d %s\n", frame_num, png_filename);
                                tdat_reset(&png_data);
                        } else {
                                /* Need more input */
                                stimeout = NULL;
                        }
                } else {
                        assert(FD_ISSET(0, &rfds));

                        /* Force next frame */
                        stimeout = &frame_timeout;

                        /* Read all available data */
                        ssize_t rlen;
                        do {
                                tdat_ensure(&input, rdsize);
                                rlen = read(0, tdat_end(&input), rdsize);
                                if (rlen > 0)
                                        input.end += rlen;
                        } while (rlen > 0);

                        if (rlen == 0) {
                                input_done = true;
                        } else if (errno == EWOULDBLOCK || errno == EINTR) {
                                /* resume from signal processing */
                        } else {
                                perror("read()");
                                break;
                        }
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
        while ((ch = getopt(ac, av, "E:R:f:")) != -1) {
                switch (ch) {
                case 'E':
                        prefix = optarg;
                        break;
                case 'R':
                        rdsize = atoi(optarg);
                        break;
                case 'f': {
                        char* ratiostart = strchr(optarg, '/');

                        fps_numerator = atol(optarg);
                        if (ratiostart)
                                fps_denominator = atol(ratiostart + 1);
                        if ((fps_numerator < 0) || (fps_denominator <= 0)) {
                                fprintf(stderr, "Invalid FPS specification\n");
                                pngext_usage(av[0]);
                                /* NOT REACHED */
                        }
                        break;
                }
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
