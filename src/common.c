#include "expr.h"
#include "common.h"



/* String Functions
 *------------------------------------------------------------------------------
 */


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
