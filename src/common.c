#include "expr.h"
#include "common.h"



/* String Functions
 *------------------------------------------------------------------------------
 */

bool lstr_eq(const LStr a, const LStr b) {
    return a.len == b.len && !strncmp(a.chars, b.chars, MIN(a.len, b.len));
}

bool lstr_str_eq(const LStr a, const char * b) {
    return !strncmp(a.chars, b, a.len);
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
    ret.mem = malloc(MAX(sizeof(void *), ARENA_BLOCK_SIZE));
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
        arena->mem = malloc(MAX(size_needed, ARENA_BLOCK_SIZE));
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
