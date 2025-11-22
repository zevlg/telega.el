#ifndef __TELEGA_DAT_H__
#define __TELEGA_DAT_H__

#include <sys/types.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>             /* free */

extern int optimize;
/* Do not send updateHavePendingNotifications events */
#define OPTIMIZE_NOTIFICATIONS 0x1
/* Remove nil values from resulting values */
#define OPTIMIZE_NIL_VALUES    0x2
/* Remove [] values from resulting values */
#define OPTIMIZE_EMPTY_LISTS   0x4
/* Remove empty strings from resulting values */
#define OPTIMIZE_EMPTY_STRINGS 0x8

struct telega_dat {
        size_t cap;             /* data capacity */

        size_t start;           /* index of first byte in data */
        size_t end;             /* index of last byte + 1 in data */
        char* data;             /* pointer to data */

        /* Func to free DATA, can be NULL */
        void (*free_data)(void *data);
};
#define TDAT_INIT { 0, 0, 0, NULL, free}
#define TDAT_INIT_VIEW(src) { (src)->cap, (src)->start, (src)->end, (src)->data, NULL }

#define tdat_len(tdat) ((tdat)->end - (tdat)->start)
#define tdat_start(tdat) (&(tdat)->data[(tdat)->start])
#define tdat_end(tdat) (&(tdat)->data[(tdat)->end])
#define tdat_drain(tdat, n) do { (tdat)->start += n; } while (0)
#define tdat_reset(tdat) (tdat)->start = (tdat)->end = 0

/* Release data in TDAT */
void tdat_drop(struct telega_dat* tdat);
/* Ensure ADD_CAP bytes can be written into TDAT */
void tdat_ensure(struct telega_dat* tdat, size_t add_cap);

/* Move N bytes from SRC into DST */
void tdat_move(struct telega_dat* src, struct telega_dat* dst, size_t n);
void tdat_append(struct telega_dat* dst, const char* data, size_t len);
#define tdat_move1(src, dst) tdat_move(src, dst, 1)
#define tdat_append1(dst, str) tdat_append(dst, str, 1)

#define tdat_append_str(dst, str) tdat_append(dst, str, strlen(str))

/* Rebase data to the beginning of the allocation */
void tdat_rebase(struct telega_dat* tdat);

#ifdef WITH_ZLIB
/* deflate SRC into DST, return number of bytes written into DST */
size_t tdat_zlib_deflate(struct telega_dat* src, struct telega_dat* dst);
#endif /* WITH_ZLIB */

/* Parsers */
void tdat_json_value(struct telega_dat* json_src, struct telega_dat* plist_dst);
void tdat_plist_value(struct telega_dat* plist_src, struct telega_dat* json_dst);

#endif  /* __TELEGA_DAT_H__ */
