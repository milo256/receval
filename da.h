#include "stddef.h"
#include "stdbool.h"

#define da(T) \
    struct { T * items; size_t len, cap; }


#define da_append(da, item) do { \
    if ((da).len >= (da).cap) { \
        size_t size = sizeof((item)) * ((da).cap? (da).cap *= 2 : ((da).cap = 1)); \
        (da).items = realloc((da).items, size); \
    } \
    (da).items[(da).len++] = (item); } while(0) \


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
