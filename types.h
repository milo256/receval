#pragma once

#define SATISFY_COMPILER 69420

#define MAX(A,B) ((A > B) ? A : B)
#define MIN(A,B) ((A > B) ? B : A)

#define ASSERT(A) do{ if (!(A)) {                                                      \
    fprintf(stderr, "Assertion failed on %s:%d: %s = %d\n", __FILE__, __LINE__, #A, A);\
    exit(1);                                                                           \
}} while(0)

#define PANIC(...) do {                                  \
    fprintf(stderr, "Panic! %s:%d\n", __FILE__, __LINE__); \
    exit(-1);                                            \
} while(0)

#define ARRLEN(A) (sof(A)/sof(A[0]))

#define ARRPUSH(item, array, len, cap) do { \
    if (++(len) > (cap)) { \
        (cap) *= 2; (array) = realloc((array), (cap) * sizeof((item))); \
    } (array)[len-1] = (item); } while(0)

typedef unsigned int u32;
typedef int i32;

typedef unsigned char u8;

typedef i32 Integer;
typedef u32 Ident;

enum VarLocation { GLOBAL, LOCAL, ARGUMENT };

typedef struct {
    char * chars;
    u32 len;
} LStr;

#define LSTR(cstr) (LStr) { .chars = cstr, .len = strlen(cstr) }


/* -- TYPES -- */
/* Note: receval does not have classes. the word "class" anywhere in this codebase just means "kind" or "category",
 * and does not have anything to do with the programming language construct. TypeClass refers to the overall type.
 * e.g. Function pointers are TypeClass `TYPE_FN_PTR` but Type `{ TYPE_FN_PTR, <return_type> }`
 */
typedef enum {
    TYPE_NONE,
    TYPE_FN_PTR,
    TYPE_INT
} TypeClass;

typedef struct Type {
    TypeClass class;
    void * data;
} Type;

typedef struct {
    Type ret_type;
    u32 args_len;    
    u32 args_cap;
    /* and then args array is stored immediately after this in memory (trust) */
}  FunctionTypeData;

/* we don't need ident because ident is just offset + location,
 * and location is which array the def is stored in */
typedef struct {
    LStr name;
    Type type;
    u32 offset;
    void * init; /* location of the initialiser in code. only used for static
                   * variables since they're initialised prior to code execution. */
    LStr * arg_names; /* only used for functions */
} Def;

/* -- TOKENS -- */
typedef enum {
    TK_INT,
    TK_FLOAT,
    TK_STRING,
    TK_FUNCTION,
    TK_ARROW,
    TK_IDENT,
    TK_ASSIGN,
    TK_OPEN,
    TK_CLOSE, 
    TK_SB_OPEN,
    TK_SB_CLOSE,
    TK_CB_OPEN,
    TK_CB_CLOSE,
    TK_EOF,
    TK_NONE
} TkClass;

typedef struct {
    TkClass class;
    LStr val;
    u32 dbug_line;
    u32 dbug_column;
} Token;

/* -- EXPRESSIONS --*/
typedef enum {
    LITERAL,
    OP_CALL,
    OP_VAR,
    OP_ASSIGN,
    OP_BUILTIN
} ExprClass;

typedef enum {
    B_ADD_I,
    B_SUB_I,
    B_MUL_I,
    B_DIV_I,
    B_IF,
    B_WHILE,
    B_SEQ,
    B_PRINT_I,
    B_NONE
} BuiltinClass;

typedef struct {
    ExprClass class;
    void * expr;
} Expr;

typedef struct { u32 localsb; Expr body; } Function;

typedef struct { Ident ident; } OpVar;
typedef struct { Ident ident; Expr val; u32 size; } OpAssign;
typedef struct { void * val; } Literal;

typedef struct {
    BuiltinClass class;
    Expr * args;
    u32 args_len;
    u32 * arg_typesb;
    u32 argsb;
} OpBuiltin;

typedef struct {
    Expr fn;
    Expr * args;
    u32 args_len;
    u32 * arg_typesb;
    u32 argsb;
} OpCall;

static u32 sof_type[] = {
    [TYPE_INT] = sizeof(Integer),
    [TYPE_FN_PTR] = sizeof(Function *)
};
