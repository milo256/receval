#pragma once

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
    TYPE_INT,
    TYPE_FN_PTR,
    TYPE_NONE
} TypeClass;

typedef struct Type {
    TypeClass class;
    struct Type * args; /* type arguments of type. A function's return type is its type's first
                         * argument and the types of its arguments follow. (do you follow?) */
    u32 args_len;
} Type;

/* we don't need ident because ident is just offset + location,
 * and location is which array the def is stored in */
typedef struct {
    LStr name;
    Type type;
    u32 offset;
    void * init; /* location of the initialiser in code. only used for static
                   * variables since they're initialised prior to code execution. */
} Def;

/* -- TOKENS -- */
typedef enum {
    TK_INT,
    TK_FUNCTION,
    TK_IDENT,
    TK_ASSIGN,
    TK_OPEN,
    TK_CLOSE,
    TK_BEGIN,
    TK_END,
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
