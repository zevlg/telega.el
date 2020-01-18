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
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "telega-dat.h"

void
tdat_ensure(struct telega_dat* tdat, size_t add_cap)
{
        while (tdat->end + add_cap > tdat->cap) {
                tdat->cap += 1 + ((add_cap > tdat->cap) ? add_cap : tdat->cap);
                tdat->data = (char*)realloc(tdat->data, tdat->cap);
                assert(tdat->data != NULL);
        }
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

void
tdat_move(struct telega_dat* src, struct telega_dat* dst, size_t n)
{
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
        case '\"':
                tdat_append1(plist, "\"");
                tdat_json_string0(json, plist, false);
                tdat_append1(plist, "\"");
                break;
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
