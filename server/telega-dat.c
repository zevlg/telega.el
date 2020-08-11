/*
 * json <--> plist converter without dependences
 *
 * Uses recursive descending techniques for conversions.
 *
 * JSON syntax from http://json.org
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *   object
 *       {}
 *       { members }
 *   members
 *       pair
 *       pair , members
 *   pair
 *       string : value
 *   array
 *       []
 *       [ elements ]
 *   elements
 *       value
 *       value , elements
 *   value
 *       string
 *       number
 *       object
 *       array
 *       true
 *       false
 *       null
 *
 * Plist syntax
 * ~~~~~~~~~~~~
 *   object
 *       ()
 *       (members)
 *   members
 *       pair
 *       pair <SPACE> members
 *   pair
 *       :keyword value
 *   vector
 *       []
 *       [ elements ]
 *   elements
 *       value
 *       value <SPACE> elements
 *   value
 *       string
 *       number
 *       object
 *       vector
 *       t
 *       :false
 *       nil
 */
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "telega-dat.h"
#include "telega-emoji.h"

void tdat_emojify_string(struct telega_dat* src_str, struct telega_dat* props);

void
tdat_ensure(struct telega_dat* tdat, size_t add_cap)
{
        while (tdat->end + add_cap > tdat->cap) {
                tdat->cap += 1 + ((add_cap > tdat->cap) ? add_cap : tdat->cap);
                tdat->data = (char*)realloc(tdat->data, tdat->cap);
        }
        assert(tdat->data != NULL);
}

void
tdat_drop(struct telega_dat* tdat)
{
        if (tdat->free_data)
                (*tdat->free_data)(tdat->data);
}

static inline char
tdat_at(struct telega_dat* tdat, size_t pos)
{
        if (tdat->start + pos < tdat->end)
                return tdat->data[tdat->start + pos];
        return 0;
}

static inline bool
tdat_has_data(struct telega_dat* tdat)
{
        return (tdat->start < tdat->end) && (tdat_at(tdat, 0) != '\0');
}

/*
 * Move N bytes from SRC to DST
 * DST could be NULL, in this case it just drain N bytes from SRC
 */
void
tdat_move(struct telega_dat* src, struct telega_dat* dst, size_t n)
{
        if (dst == NULL) {
                tdat_drain(src, n);
                return;
                /* NOT REACHED */
        }

        assert(src->start + n <= src->end);

        tdat_ensure(dst, dst->end + n);
        memcpy(&dst->data[dst->end], &src->data[src->start], n);
        dst->end += n;
        src->start += n;
}

void
tdat_append(struct telega_dat* dst, const char* data, size_t len)
{
        tdat_ensure(dst, dst->end + len);
        memcpy(&dst->data[dst->end], data, len);
        dst->end += len;
}

void
tdat_rebase(struct telega_dat* tdat)
{
        size_t clen = tdat_len(tdat);
        memmove(tdat->data, &tdat->data[tdat->start], clen);
        tdat->start = 0;
        tdat->end = clen;
}


/* JSON */
static void
tdat_json_whitespaces(struct telega_dat* src)
{
        while (tdat_has_data(src)) {
                if (!isspace(tdat_at(src, 0)))
                        break;
                tdat_drain(src, 1);
        }
}

static void
tdat_json_string0(struct telega_dat* src, struct telega_dat* dst, bool no_spaces)
{
        assert(tdat_at(src, 0) == '"');

        tdat_drain(src, 1);     /* " */
        while (tdat_has_data(src)) {
                char c = tdat_at(src, 0);
                if (c == '\"') {
                        tdat_drain(src, 1);
                        return;
                }

                if (c == '\\') {
                        tdat_move1(src, dst);
                        if (!tdat_has_data(src))
                                break;
                }

                if (no_spaces && isspace(c)) {
                        fprintf(stderr, "Space in string is not allowed\n");
                        assert(false);
                }

                tdat_move1(src, dst);
        }
}

static void
tdat_json_object(struct telega_dat* json, struct telega_dat* plist)
{
        tdat_append1(plist, "(");

        assert(tdat_at(json, 0) == '{');
        tdat_drain(json, 1);    /* { */
        while (tdat_has_data(json)) {
                tdat_json_whitespaces(json);
                switch (tdat_at(json, 0)) {
                case '}':
                        tdat_append1(plist, ")");
                        tdat_drain(json, 1);
                        return;
                case ':':
                        tdat_drain(json, 1); /* : */
                        tdat_append1(plist, " ");
                        tdat_json_value(json, plist);
                        break;
                case ',':
                        tdat_append1(plist, " ");
                        tdat_drain(json, 1); /* , */
                        tdat_json_whitespaces(json);
                        /* FALLTHROUGH */
                case '"':
                        tdat_append1(plist, ":");
                        tdat_json_string0(json, plist, true);
                        break;
                default:
                        fprintf(stderr, "Unexpected char '%c' in json object\n",
                                tdat_at(json, 0));
                        assert(false);
                        /* NOT REACHED */
                }
        }
}

static void
tdat_json_array(struct telega_dat* json, struct telega_dat* plist)
{
        assert(tdat_at(json, 0) == '[');

        tdat_move1(json, plist); /* [ */
        while (tdat_has_data(json)) {
                tdat_json_whitespaces(json);
                switch (tdat_at(json, 0)) {
                case ']':
                        tdat_move1(json, plist); /* ] */
                        return;
                case ',':
                        tdat_append1(plist, " ");
                        tdat_drain(json, 1);
                        break;
                default:
                        tdat_json_value(json, plist);
                        break;
                }
        }
}

static void
tdat_json_number(struct telega_dat* json, struct telega_dat* plist)
{
        while (tdat_has_data(json)) {
                switch (tdat_at(json, 0)) {
                case '-':
                case '+':
                case '0' ... '9':
                case 'e':
                case 'E':
                case '.':
                        tdat_move1(json, plist);
                        break;
                default:
                        return;
                }
        }
}

void
tdat_json_value(struct telega_dat* json, struct telega_dat* plist)
{
        tdat_json_whitespaces(json);
        switch (tdat_at(json, 0)) {
        case '{':
                tdat_json_object(json, plist);
                break;
        case '[':
                tdat_json_array(json, plist);
                break;
        case '\"': {
                struct telega_dat pstr = TDAT_INIT;
                struct telega_dat props = TDAT_INIT;
                tdat_json_string0(json, &pstr, false);
                tdat_emojify_string(&pstr, &props);
                if (tdat_has_data(&props)) {
                        tdat_append_str(plist, "#(\"");
                        tdat_move(&pstr, plist, tdat_len(&pstr));
                        tdat_append1(plist, "\"");
                        tdat_move(&props, plist, tdat_len(&props));
                        tdat_append1(plist, ")");
                } else {
                        tdat_append1(plist, "\"");
                        tdat_move(&pstr, plist, tdat_len(&pstr));
                        tdat_append1(plist, "\"");
                }
                tdat_drop(&pstr);
                tdat_drop(&props);
                break;
        }
        case '-':
        case '0' ... '9':
                tdat_json_number(json, plist);
                break;
        case 't':               /* true */
                tdat_drain(json, 4);
                tdat_append1(plist, "t");
                break;
        case 'f':               /* false */
                tdat_drain(json, 5);
                tdat_append(plist, "nil", 3);
                break;
        case 'n':               /* null */
                tdat_drain(json, 4);
                tdat_append(plist, "nil", 3);
                break;
        default:
                fprintf(stderr, "Unexpected char '%c' in json value\n",
                        tdat_at(json, 0));
                assert(false);
        }
        tdat_json_whitespaces(json);
}


/* PLIST */
#define tdat_plist_string0 tdat_json_string0
#define tdat_plist_number tdat_json_number
#define tdat_plist_whitespaces tdat_json_whitespaces

static void
tdat_plist_keyword(struct telega_dat* plist, struct telega_dat* json)
{
        tdat_append1(json, "\"");

        assert(tdat_at(plist, 0) == ':');
        tdat_drain(plist, 1);   /* : */
        while (tdat_has_data(plist)) {
                if (isspace(tdat_at(plist, 0)))
                        break;
                tdat_move1(plist, json);
        }

        tdat_append1(json, "\"");
}

static void
tdat_plist_object(struct telega_dat* plist, struct telega_dat* json)
{
        tdat_append1(json, "{");

        assert(tdat_at(plist, 0) == '(');
        tdat_drain(plist, 1); /* ( */
        tdat_plist_whitespaces(plist);
        while (tdat_has_data(plist)) {
                switch (tdat_at(plist, 0)) {
                case ')':
                        tdat_append1(json, "}");
                        tdat_drain(plist, 1);
                        return;
                case ' ':
                        tdat_drain(plist, 1);
                        tdat_append1(json, ",");
                        break;
                case ':':
                        tdat_plist_keyword(plist, json);
                        tdat_append1(json, ":");
                        tdat_plist_value(plist, json);
                        break;
                default:
                        fprintf(stderr, "Invalid plist object at pos=%zu\n",
                                plist->start);
                        assert(false);
                }
        }
}

static void
tdat_plist_vector(struct telega_dat* plist, struct telega_dat* json)
{
        assert(tdat_at(plist, 0) == '[');
        tdat_move1(plist, json); /* [ */

        tdat_plist_whitespaces(plist);
        while (tdat_has_data(plist)) {
                switch (tdat_at(plist, 0)) {
                case ']':
                        tdat_move1(plist, json); /* ] */
                        return;
                case ' ':
                        tdat_append1(json, ",");
                        tdat_plist_whitespaces(plist);
                        break;
                default:
                        tdat_plist_value(plist, json);
                        break;
                }
        }
}

void
tdat_plist_value(struct telega_dat* plist, struct telega_dat* json)
{
        tdat_plist_whitespaces(plist);
        switch (tdat_at(plist, 0)) {
        case '(':
                tdat_plist_object(plist, json);
                break;
        case '[':
                tdat_plist_vector(plist, json);
                break;
        case '\"':
                tdat_append1(json, "\"");
                tdat_plist_string0(plist, json, false);
                tdat_append1(json, "\"");
                break;
        case '-':
        case '0' ... '9':
                tdat_plist_number(plist, json);
                break;
        case 't':               /* t */
                tdat_drain(plist, 1);
                tdat_append(json, "true", 4);
                break;
        case ':':               /* :false */
                tdat_drain(plist, 6);
                tdat_append(json, "false", 5);
                break;
        case 'n':               /* nil */
                tdat_drain(plist, 3);
                tdat_append(json, "null", 4);
                break;
        default:
                fprintf(stderr, "Unexpected char '%c' in plist value\n",
                        tdat_at(plist, 0));
                assert(false);
                /* NOT REACHED */
        }
}

/* UTF16 code points */
static inline int
hexc2int(int c)
{
        if ((c >= '0') && (c <= '9'))
                return c - '0';
        else if ((c >= 'a') && (c <= 'f'))
                return c - 'a' + 10;
        else if ((c >= 'A') && (c <= 'F'))
                return c - 'A' + 10;
        else
                assert(false);
        return -1;
}

/**
 * Read UTF16 code point in \uXXXX form
 * Return newly read character
 */
static uint32_t
tdat_move_utf16codepoint(struct telega_dat* src, struct telega_dat* dst)
{
        uint32_t c0 = (uint32_t)tdat_at(src, 0);
        if (c0 != '\\') {
                /* Unescaped char */
                tdat_move1(src, dst);
                return c0;
                /* NOT REACHED */
        }

        assert(tdat_len(src) > 1);
        char c1 = tdat_at(src, 1);
        tdat_move(src, dst, 2);

        switch (c1) {
        case '\\':
                return (uint32_t)'\\';
        case 'n':
                return (uint32_t)'\n';
        case 'r':
                return (uint32_t)'\r';
        case 't':
                return (uint32_t)'\t';
        case 'b':
                return (uint32_t)'\b';
        case '\'':
                return (uint32_t)'\'';
        case '"':
                return (uint32_t)'\"';
        case 'u': {
                char u0 = tdat_at(src, 0);
                char u1 = tdat_at(src, 1);
                char u2 = tdat_at(src, 2);
                char u3 = tdat_at(src, 3);
                tdat_move(src, dst, 4);

                return hexc2int(u3) | (hexc2int(u2) << 4)
                        | (hexc2int(u1) << 8) | (hexc2int(u0) << 12);
        }
        default:
                fprintf(stderr, "Unsupported escape char '\\%c'\n", c1);
                assert(false);
        }
        return -1;
}

/**
 * Extract UTF16 char, possible dessurogating pairs
 */
uint32_t
tdat_move_utf16char(struct telega_dat* src, struct telega_dat* dst)
{
        /* NOTE: we save dst->end in case for surrogate pair in SRC.
         * In this case, we restore dst_end and put unicode char
         * (with \U prefix) into dst
         */
        size_t saved_dst_end = dst ? dst->end : 0;

        uint32_t high = tdat_move_utf16codepoint(src, dst);
        if ((high >= 0xD800) && (high <= 0xDBFF)) {
                uint32_t low = tdat_move_utf16codepoint(src, dst);
                if ((low >= 0xDC00) && (low <= 0xDFFF)) {
                        high = (high - 0xD800) << 10;
                        high += 0x10000 + (low - 0xDC00);

                        char high_hex[11];
                        snprintf(high_hex, 11, "\\U%08x", high);

                        if (dst) {
                                dst->end = saved_dst_end;
                                tdat_append_str(dst, high_hex);
                        }
                }
        }
        return high;
}

/* Return len in utf16 codepoints for char C */
static inline int
utf16_clen(uint32_t c)
{
        return (c > 0xFFFF) ? 2 : 1;
}

/*
 * Read emoji sequence from SRC to DST, consuming utf16 chars
 * Return number of utf16 chars placed into DST
 */
size_t
tdat_move_emoji_sequence(struct telega_dat* src, struct telega_dat* dst,
                         const struct emoji_trie* emoji_trie)
{
        if (!tdat_has_data(src))
                return 0;

        struct telega_dat ret = TDAT_INIT;
        size_t start_from = src->start;

        uint32_t ch = tdat_move_utf16char(src, &ret);

        assert(emoji_trie != NULL);
        const struct emoji_trie* et;
        for (et = &emoji_trie[0]; et->match; et++) {
                if ((et->match != EMOJI_MATCH_ANY) && (ch != et->match))
                        continue;

                size_t cn = 0;
                /* Try longest match first */
                if ((et->childs
                     && (cn = tdat_move_emoji_sequence(src, &ret, et->childs)))
                    || et->is_terminal)
                {
                        /* FOUND */
                        size_t nbytes = tdat_len(&ret);
                        tdat_move(&ret, dst, nbytes);
                        return cn + utf16_clen(ch);
                        /* NOT REACHED */
                }
        }

        /* Reset SRC to initial state */
        src->start = start_from;

        tdat_drop(&ret);
        return 0;
}

#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif /* MIN */

static void
tdat_emojify_append_property(struct telega_dat* props, size_t start,
                             size_t end, struct telega_dat* disp)
{
        char prop_position[33];
        snprintf(prop_position, 33, " %zu %zu", start, end);
        tdat_append_str(props, prop_position);
        tdat_append_str(props, " (telega-emoji-p t telega-display \"");
        tdat_move(disp, props, tdat_len(disp));
        tdat_append_str(props, "\")");
}

/**
 * Extract emojis from SRC and put corresponding properties into PROPS
 * Return non-false if any of the property has been extracted.
 */
void
tdat_emojify_string(struct telega_dat* src_str, struct telega_dat* props)
{
        struct telega_dat src_view = TDAT_INIT_VIEW(src_str);
        struct telega_dat disp = TDAT_INIT;

        /* value for src_view->start for backtracking */
        size_t backtrack[3];
        size_t backtrack_size = 0;

        /* Offsets in SRC_VIEW in utf16 chars */
        size_t backtrack_offsets[3];
        size_t offset = 0;

        while (tdat_has_data(&src_view)) {
                /* update the backtrack */
                backtrack[2] = backtrack[1];
                backtrack[1] = backtrack[0];
                backtrack[0] = src_view.start;
                backtrack_offsets[2] = backtrack_offsets[1];
                backtrack_offsets[1] = backtrack_offsets[0];
                backtrack_offsets[0] = offset;
                if (backtrack_size < 3)
                        backtrack_size++;

                struct emoji_match_table* tables = NULL;
                uint32_t ch = tdat_move_utf16char(&src_view, NULL);
                offset += utf16_clen(ch);
                if ((0x231A <= ch) && (ch <= 0x2B55))
                        tables = emoji_basic1_tables;
                else if (ch == 0xFE0F)
                        tables = emoji_other_tables; /* fe0f */
                else if ((0x1F1E6 <= ch) && (ch <= 0x1F1FF))
                        tables = emoji_other_tables; /* flags */
                else if (ch == 0x1F3F4)
                        tables = emoji_other_tables; /* tags */
                else if ((0x1F3FB <= ch) && (ch <= 0x1F3FF))
                        tables = emoji_other_tables; /* modifiers */
                else if (ch == 0x200D)
                        tables = emoji_other_tables; /* zwj */
                else if ((0x1F004 <= ch) && (ch <= 0x1FAD6))
                        tables = emoji_basic2_tables;
                else
                        tables = emoji_null_tables;

                bool found = false;
                for (; tables->match_trie; tables++) {
                        if ((ch < tables->min_match) || (ch > tables->max_match))
                                continue;

                        size_t saved_start = src_view.start;

                        /** NOTE: Start backtracking from index
                         * specified in table
                         */
                        int bti_size = MIN(backtrack_size,
                                           tables->max_backtrack + 1);
                        for (int bti = 0; bti < bti_size; bti++) {
                                int btindex = bti_size - bti - 1;
                                src_view.start = backtrack[btindex];
                                size_t coff = backtrack_offsets[btindex];

                                tdat_reset(&disp);
                                size_t cn = tdat_move_emoji_sequence(
                                        &src_view, &disp, tables->match_trie);
                                if (cn) {
                                        /* MATCHED */
                                        found = true;
                                        offset = coff + cn;

                                        tdat_emojify_append_property(
                                                props, coff, offset, &disp);
                                        break;
                                        /* NOT REACHED */
                                }
                        }

                        if (found)
                                break;

                        src_view.start = saved_start;
                }

                /* NOTE: always mark surrogated pairs as emoji, even
                 * if not found in emoji tables, so surrogated pairs
                 * won't apper in strings ever
                 */
                if (!found && (utf16_clen(ch) == 2)) {
                        src_view.start = backtrack[0];
                        tdat_reset(&disp);
                        tdat_move_utf16char(&src_view, &disp);

                        assert(offset >= 2);
                        tdat_emojify_append_property(
                                props, offset - 2, offset, &disp);
                }
        }

        tdat_drop(&disp);
}
