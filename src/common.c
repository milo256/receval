#include "expr.h"
#include "common.h"
#include <stdarg.h>



/* String Functions
 *------------------------------------------------------------------------------
 */

int slice_cmp(slice_t a, slice_t b) {
    u32 alen = slicelen(a), blen = slicelen(b);
    u32 iters = min(alen, blen);
    for (u32 i = 0; i < iters;i++)
        if (a.sptr[i] != b.sptr[i])
            return (a.sptr[i] > b.sptr[i]) - (a.sptr[i] < b.sptr[i]);
    if (alen && !blen) return 1;
    if (blen && !alen) return -1;
    return 0;
}

bool slice_eq(slice_t a, slice_t b) {
    u32 alen = slicelen(a), blen = slicelen(b);
    if (alen != blen) return 0;
    for (u32 i = 0; i < alen;i++)
        if (a.sptr[i] != b.sptr[i]) return 0;
    return 1;
}


bool slice_str_eq(slice_t a, char * b) {
    u32 i;
    for (i = 0; i < slicelen(a);i++)
        if (a.sptr[i] != b[i]) return 0;
        else if (!b[i]) return 0;
    if (!b[i]) return 1; 
    else return 0;
}


slice_t slice(char * str) {
    char *sptr = str, * eptr = str;
    for (; *eptr; eptr++);
    return (slice_t) { sptr, eptr };
}


size_t slicelen(slice_t slice) {
    return slice.eptr - slice.sptr;
}


size_t slice_to_nullt(char * buf, slice_t slice) {
    size_t len = slicelen(slice);
    for (size_t i = 0; i < len; i++)
        buf[i] = slice.sptr[i];
    buf[len] = 0;
    return len;
}


#define first_n_bits(val, n) (((val) >> (sizeof(val) * 8 - n) & ((1 << (n)) - 1)))

int codepoint_len(char sbyte) {
    if (first_n_bits(sbyte, 1) == 0) return 1;
    if (first_n_bits(sbyte, 3) == 0b110) return 2;
    if (first_n_bits(sbyte, 4) == 0b1110) return 3;
    if (first_n_bits(sbyte, 5) == 0b11110) return 4;
    return NOT_CODEPOINT; 
}


iterdef(i_split, size_t, slice_t) (size_t * sidx, slice_t * item, slice_t str, char sep) {
    char * eptr = str.sptr + *sidx;
    while (eptr <= str.eptr)
        if (*eptr == sep || eptr == str.eptr) {
            *item = (slice_t) { str.sptr + *sidx, eptr };
            *sidx = eptr - str.sptr + 1;
            return true;
        } else (eptr++);
    return false;
}


iterdef(i_codepoints, size_t, slice_t) (size_t * sidx, slice_t * item, slice_t str) {
    int len;
    char * sptr = str.sptr + *sidx;
    do
        if (sptr >= str.eptr) return false;
        else len = codepoint_len(*sptr);
    while (len == NOT_CODEPOINT && sptr++);

    *sidx = sptr + len - str.sptr;
    return *item = (slice_t) { sptr, sptr + len }, true;
}




static bool exfmt_bsearch(formatter_t fmt, slice_t name, u32 * index) {
    /* TODO: binary search */
    for (*index = 0; *index < fmt.len; (*index)++) {
        int cmp = slice_cmp(fmt.items[*index].name, name);
        if (cmp == 0) return true;
        if (cmp < 0) return false;
    }
    return false;
}


void exfmt_ex(formatter_t * fmt, slice_t name, exfmt_fn_t fn) {
    for (u32 i = 0; i < slicelen(name); i++)
        assert(name.sptr[i] != '}');
    exfmt_t new = { name, fn };
    da_next(*fmt);
    u32 i;
    assert(!exfmt_bsearch(*fmt, name, &i) /* name already exists */);
    for (int j = fmt->len - 2; j >= (int) i; j++)
        fmt->items[j + 1] = fmt->items[j];
    fmt->items[i] = new;
}



void exfmt_vpda(formatter_t fmt, dslice_t * buf, char * fmtstr, va_list args) {
    slice_t str = slice(fmtstr);
    if (slicelen(str) < 2) {
        da_append_str(*buf, fmtstr); 
        return;
    }

    for (char * chptr = fmtstr; *chptr; chptr++) {
        if (!slice_str_eq((slice_t) { chptr, chptr + 2 }, "${"))
            da_append(*buf, *chptr);
        else {
            slice_t name = { chptr + 2, chptr + 4 };
            while (*name.eptr && *name.eptr != '}')
                name.eptr++;
            assert(*name.eptr /* missing '}' */);
            u32 i;
            assert(exfmt_bsearch(fmt, name, &i) /* name doesn't exist */);
            void * value = va_arg(args, void *);
            fmt.items[i].fn(buf, value);
            chptr = name.eptr;
        }
    }
}


void exfmt_pda(formatter_t fmt, dslice_t * buf, char * fmtstr, ...) {
    va_list args;
    va_start(args, fmtstr);
    exfmt_vpda(fmt, buf, fmtstr, args);
    va_end(args);
}


char * exfmt_vpds(formatter_t fmt, char * fmtstr, va_list args) {
    dslice_t buf = {};
    exfmt_vpda(fmt, &buf, fmtstr, args);
    da_append(buf, 0);
    return buf.items;
}


char * exfmt_pds(formatter_t fmt, char * fmtstr, ...) {
    va_list args;
    va_start(args, fmtstr);
    char * ret = exfmt_vpds(fmt, fmtstr, args);
    va_end(args);
    return ret;
}


void exfmt_vp(formatter_t fmt, char * fmtstr, va_list args) {
    char * str, * ch = exfmt_vpds(fmt, fmtstr, args);
    str = ch;
    for (; *ch; ch++)
        putchar(*ch);
    free(str);
}


void exfmt_p(formatter_t fmt, char * fmtstr, ...) {
    va_list args;
    va_start(args, fmtstr);
    exfmt_vp(fmt, fmtstr, args);
    va_end(args);
}


void exfmt_dealloc(formatter_t * fmt) {
    da_dealloc(*fmt);
}


/* Arena Functions
 *------------------------------------------------------------------------------
 */

#ifndef __NDEBUG__
#ifdef __SANITIZE_ADDRESS__
#define ARENA_DEBUG
#endif
#endif

#ifdef ARENA_DEBUG
#define ARENA_BLOCK_SIZE 0
#else
#define ARENA_BLOCK_SIZE 4096
#endif

Arena arena_init(void) {
    Arena ret;
    ret.mem = malloc(max(sizeof(void *), ARENA_BLOCK_SIZE));
    *(void **) ret.mem = NULL;
    ret.fill_ptr = ret.mem + sizeof(void *);
    return ret;
}

void * aalloc(Arena * arena, u32 size) {
    void * ptr;
    if (arena->fill_ptr + size < arena->mem + ARENA_BLOCK_SIZE) {
        ptr = arena->fill_ptr;
    } else {
        void * old_mem = arena->mem;
        u32 size_needed = size + sizeof(void *);
        arena->mem = malloc(max(size_needed, ARENA_BLOCK_SIZE));
        *(void **) arena->mem = old_mem;
        ptr = arena->mem + sizeof(void *);
    }
    arena->fill_ptr = ptr + size;
    return ptr;
}

static void arena_mem_free(void * arena_mem) {
    void * last = *(void **) arena_mem;
    //printf("last: %p\n", last);
    if (last) arena_mem_free(last);
    free(arena_mem);
}

void afree(Arena arena) { if (arena.mem) arena_mem_free(arena.mem); }



/*------------------------------------------------------------------------------
 * Ident Functions
 */

Ident ident_new(u32 loc, u32 ofs) { return (loc << 30) | ofs; }
u32 var_location(Ident ident) { return ((3 << 30) & ident) >> 30; }
u32 var_offset(Ident ident) { return (((1 << 30) - 1) & ident); }
