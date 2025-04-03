#include "common.h"
#include "expr.h"

/* Receval Types
 * -----------------------------------------------------------------------------
 */

#define DEF_PRIMITIVE_TYPES(f) \
/*  | enum name | str name | shorthand | size */\
    f(TYPE_VOID,  "void",    'n',        0) \
    f(TYPE_INT,   "int",     'i',        sizeof(Integer)) \
    f(TYPE_STR,   "str",     's',        sizeof(String))

#define DEF_COMPLEX_TYPES(f) \
/*  | enum name      | str name | shorthand | size */\
    f(TYPE_FUNCTION,   NULL,      'p',        sizeof(int *))

#define DEF_TYPES(f) \
    DEF_PRIMITIVE_TYPES(f) DEF_COMPLEX_TYPES(f)

#define TC_ENUM(en, sn, sh, sz) en,
#define TYPE_SIZE_TABLE(en, sn, sh, sz) [en] = sz,
#define TYPE_NAME_TABLE(en, sn, sh, sz) [en] = sn,
#define TYPE_SH_TABLE(en, sn, sh, sz) [en] = sh,

typedef enum {
    TYPE_NONE,
    DEF_PRIMITIVE_TYPES(TC_ENUM)
    FIRST_COMPLEX_TYPE,
    LAST_PRIMITIVE_TYPE = FIRST_COMPLEX_TYPE - 1,
    DEF_COMPLEX_TYPES(TC_ENUM)
    TYPE_UNKNOWN, /* TYPE_NONE represents an error state. The type was formatted
                   * wrong or something. TYPE_UNKNOWN is when the type exists
                   * but hasn't been determined yet. Used for type inference.
                   */
    NTYPES
} TypeClass;


static u32 type_sizes[] = { DEF_TYPES(TYPE_SIZE_TABLE) };
static char * type_names[] = { DEF_TYPES(TYPE_NAME_TABLE) };
static char type_shorthands[] = { DEF_TYPES(TYPE_SH_TABLE) };


typedef struct Type {
    TypeClass class;
    void * data;
} Type;


typedef struct {
    Type ret_type;
    u32 param_count;    
    /* and then args array is stored immediately after this in memory (trust) */
}  FunctionTypeData;

