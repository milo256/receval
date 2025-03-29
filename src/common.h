#pragma once
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>


typedef uint32_t u32;
typedef int32_t i32;
typedef uint8_t u8;
typedef int8_t i8;


#define TYPE_MAX_SIZE (2 * sizeof(void *))



/* Common Macros
 * -----------------------------------------------------------------------------
 */

#define MAX(A,B) ((A > B) ? A : B)

#define MIN(A,B) ((A > B) ? B : A)

#define ARRLEN(A) (sizeof(A)/sizeof(A[0]))

#ifdef NDEBUG
#define ASSERT(A) ((void) 0)
#define ASSERT_EQ(A, B) ((void) 0)
#else

#define ASSERT_EQ(A, B) if ((A) != (B)) { \
    fprintf( \
        stderr, "Assertion failed on %s:%d: %s (%ld) != %s (%ld)\n", \
        __FILE__, __LINE__, #A, ((long) A), #B, ((long) B) \
    ); \
    exit(-1); \
}


#define ASSERT(A) do{ if (!(A)) {                                              \
    fprintf(stderr, "Assertion failed on %s:%d: %s\n", __FILE__, __LINE__, #A);\
    exit(-1);                                                                  \
} } while(0)

#endif


#define PANIC(...) do { \
    fprintf(stderr, "Panic! %s:%d", __FILE__, __LINE__); \
    fprintf(stderr," " __VA_ARGS__); \
    fprintf(stderr,"\n"); \
    exit(-1); \
} while(0)

#define unreachable \
    PANIC("unreachable");



/* String Slices
 * -----------------------------------------------------------------------------
 */

typedef struct {
    char * chars;
    u32 len;
} LStr;

#define LSTR(cstr) (LStr) { .chars = cstr, .len = strlen(cstr) }

bool lstr_str_eq(const LStr a, const char * b);
bool lstr_eq(const LStr a, const LStr b);



/* Memory Arenas
 * -----------------------------------------------------------------------------
 */

typedef struct {
    void * mem, * fill_ptr;
} Arena;

Arena arena_init(void);
void * aalloc(Arena *, u32);
void afree(Arena);



/* Dynamic Arrays
 * -----------------------------------------------------------------------------
 */

#define da(T) \
    struct { T * items; size_t len, cap; }


#define da_append(da, item) do { \
    if ((da).len >= (da).cap) { \
        size_t size = sizeof((item)) * ((da).cap? (da).cap *= 2 : ((da).cap = 1)); \
        (da).items = realloc((da).items, size); \
    } \
    (da).items[(da).len++] = (item); } while(0) \


#define da_next(da) (\
        ( (da).len < (da).cap )?: \
            ( (da).items = realloc( \
                (da).items, sizeof((da).items[0]) * ((da).cap? (da).cap *= 2 : ((da).cap = 1)) \
            ) ), \
        &(da).items[(da).len++] \
    ) \


#define da_foreach(item_var, da) \
    if ((da).items) for(typeof((da).items[0]) * item_var = (da).items;  item_var < (da).items + (da).len; item_var++)


#define da_eforeach(index_var, item_var, da) \
    if ((da).items) for(bool ms__latch = 1; ms__latch;) \
    for(typeof((da).items[0]) * item_var = &(da).items[0]; ms__latch; ms__latch = 0) \
    for(size_t index_var = 0; index_var < (da).len; index_var++, item_var = (da).items + index_var)


#define da_dealloc(da) \
    (free((da).items), (da).len = (da).cap = 0, (da).items = NULL)


#define da_resize(da, new_cap) ( \
        (da).len = (((da).cap = (new_cap)) < (da).len)? (new_cap) : (da).len, \
        (da).items = realloc((da).items, (new_cap) * sizeof((da).items[0])) \
    )


#define da_new(type, cap) { malloc(cap * sizeof(type)), 0, cap }

