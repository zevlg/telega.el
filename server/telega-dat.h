#ifndef __TELEGA_DAT_H__
#define __TELEGA_DAT_H__

#include <stdbool.h>
#include <assert.h>
#include <sys/types.h>

struct telega_dat {
        size_t cap;             /* data capacity */

        size_t start;           /* index of first byte in data */
        size_t end;             /* index of last byte + 1 in data */
        char* data;             /* pointer to data */
};
#define TDAT_INIT { 0, 0, 0, NULL}

#define tdat_len(tdat) ((tdat)->end - (tdat)->start)
#define tdat_start(tdat) (&(tdat)->data[(tdat)->start])
#define tdat_end(tdat) (&(tdat)->data[(tdat)->end])
#define tdat_drain(tdat, n) do { (tdat)->start += n; } while (0)

/* Ensure ADD_CAP bytes can be written into TDAT */
void tdat_ensure(struct telega_dat* tdat, size_t add_cap);
#define tdat_reset(tdat) (tdat)->start = (tdat)->end = 0
#define tdat_drop(tdat) free((tdat)->data)

/* Move N bytes from SRC into DST */
void tdat_move(struct telega_dat* src, struct telega_dat* dst, size_t n);
void tdat_append(struct telega_dat* dst, const char* data, size_t len);
#define tdat_move1(src, dst) tdat_move(src, dst, 1)
#define tdat_append1(dst, str) tdat_append(dst, str, 1)

/* Rebase data to the beginning of the alocation */
void tdat_rebase(struct telega_dat* tdat);

/* Parsers */
void tdat_json_value(struct telega_dat* json_src, struct telega_dat* plist_dst);
void tdat_plist_value(struct telega_dat* plist_src, struct telega_dat* json_dst);

#endif  /* __TELEGA_DAT_H__ */
