#pragma once
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>


typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;



#define TYPE_MAX_SIZE (2 * sizeof(void *))


/* Common Macros
 * -----------------------------------------------------------------------------
 */

#define max(A,B) (((A) > (B)) ? (A) : (B))

#define min(A,B) (((A) < (B)) ? (A) : (B))

#define arrlen(A) (sizeof(A)/sizeof(A[0]))

#ifdef NDEBUG
#define ASSERT(A) ((void) 0)
#define ASSERT_EQ(A, B) ((void) 0)
#else

#define assert_eq(A, B) if ((A) != (B)) { \
    fprintf( \
        stderr, "Assertion failed on %s:%d: %s (%ld) != %s (%ld)\n", \
        __FILE__, __LINE__, #A, ((long) A), #B, ((long) B) \
    ); \
    exit(-1); \
}


#define assert(A) do{ if (!(A)) {                                              \
    fprintf(stderr, "Assertion failed on %s:%d: %s\n", __FILE__, __LINE__, #A);\
    exit(-1);                                                                  \
} } while(0)

#endif


#define panic(...) do { \
    fprintf(stderr, "Panic! %s:%d", __FILE__, __LINE__); \
    fprintf(stderr," " __VA_ARGS__); \
    fprintf(stderr,"\n"); \
    exit(-1); \
} while(0)

#define unreachable \
    panic("unreachable");



/* Memory Arenas
 * -----------------------------------------------------------------------------
 */

typedef struct {
    void * mem, * fill_ptr;
} Arena;

Arena arena_init(void);
void * aalloc(Arena *, u32);
void afree(Arena);



/* Iterators
 * -----------------------------------------------------------------------------
 */

#define iterdef(name, state_t, item_t) \
    typedef item_t name##_item_t; \
    typedef state_t name##_state_t; \
    bool name


#define iterate(item_var, iterator, ...) \
    for(bool i__latch = 1; i__latch;) \
    for(iterator##_item_t item_var; i__latch; i__latch = 0) \
    for(iterator##_state_t i__iterstate = {}; iterator(&i__iterstate, &item_var, __VA_ARGS__);)


#define iternumerate(index_var, item_var, iterator, ...) \
    for(bool ms__latch = 1; ms__latch;) \
    for(size_t index_var; ms__latch;) \
    for(iterator##_item_t item_var; ms__latch; ms__latch = 0) \
    for(iterator##_state_t ms__iterstate = {}; index_var++, iterator(&ms__iterstate, &item_var, __VA_ARGS__);)



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
        ( (da).len < (da).cap )? 0 : \
            (void) ( (da).items = realloc( \
                (da).items, sizeof((da).items[0]) * ((da).cap? (da).cap *= 2 : ((da).cap = 1)) \
            ) ), \
        &(da).items[(da).len++] \
        )



#define da_foreach(item_type, item_var, da) \
    if ((da).items) for(item_type * item_var = (da).items;  item_var < (da).items + (da).len; item_var++)


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


#define da_append_arr(da, arr) (\
        da_resize((da), max((da).cap, (da).len + arrlen(arr))), \
        memcpy((da).items + (da).len, (arr), arrlen(arr) * sizeof(arr[0])), \
        (da).len += arrlen(arr), \
        arrlen(arr) \
    )


#define da_append_str(da, cstr) do {\
        u32 len = strlen(cstr); \
        da_resize((da), max((da).cap, (da).len + len)); \
        memcpy((da).items + (da).len, (cstr), len); \
        (da).len += len; } while(0) \



/* String Slices
 * -----------------------------------------------------------------------------
 */

typedef struct {
    char * sptr;
    char * eptr;
} slice_t;


typedef da(char) dslice_t;



enum {
    NOT_CODEPOINT = -1
};


slice_t slice(char * str);

size_t slicelen(slice_t slice);

size_t slice_to_nullt(char * buf, slice_t slice);

int codepoint_len(char sbyte);

bool slice_eq(slice_t a, slice_t b);

bool slice_str_eq(slice_t a, char * b);

iterdef(i_split, size_t, slice_t) (size_t * sidx, slice_t * item, slice_t str, char sep);

iterdef(i_codepoints, size_t, slice_t) (size_t * sidx, slice_t * item, slice_t str);




/* Extensible Formatter
 * -----------------------------------------------------------------------------
 */

typedef void (*exfmt_fn_t)(dslice_t *, void *);

typedef struct {
    slice_t name;
    exfmt_fn_t fn;
} exfmt_t;


typedef da(exfmt_t) formatter_t;


void exfmt_ex(formatter_t * fmt, slice_t name, exfmt_fn_t fn);

void exfmt_dealloc(formatter_t * fmt);

void exfmt_vpda(formatter_t fmt, dslice_t * buf, char * fmtstr, va_list args);
void exfmt_pda(formatter_t fmt, dslice_t * buf, char * fmtstr, ...);
char * exfmt_vpds(formatter_t fmt, char * fmtstr, va_list args);
char * exfmt_pds(formatter_t fmt, char * fmtstr, ...);
void exfmt_vp(formatter_t fmt, char * fmtstr, va_list args);
void exfmt_p(formatter_t fmt, char * fmtstr, ...);
