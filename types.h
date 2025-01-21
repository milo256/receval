#pragma once


typedef unsigned int u32;
typedef int i32;

typedef unsigned char u8;

typedef i32 Integer;
typedef u32 Ident;

enum VarLocation { GLOBAL, LOCAL, ARGUMENT };

/* -- TYPES -- */
/* Note: receval does not have classes. the word "class" anywhere in this codebase just means "kind" or "category",
 * and does not have anything to do with the programming language construct. TypeClass refers to the overall type.
 * e.g. Function pointers are TypeClass `TYPE_FN_PTR` but Type `{ TYPE_FN_PTR, <return_type> }`
 */
typedef enum {
    TYPE_INT,
    TYPE_FN_PTR,
} TypeClass;

typedef struct Type {
    TypeClass class;
    union { struct Type * ret_type; };
} Type;

/* we don't need ident because ident is just offset + location,
 * and location is which array the def is stored in */
typedef struct {
    char * name;
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
    TK_EOF
} TkClass;

typedef struct { TkClass class; char * value; } Token;

/* -- EXPRESSIONS --*/
typedef enum {
    LITERAL,
    OP_CALL,
    OP_VAR,
    OP_ASSIGN,
    OP_BUILTIN
} ExprClass;

typedef enum {
    B_ADD,
    B_SUBTRACT,
    B_MULTIPLY,
    B_DIVIDE,
    B_IF,
    B_WHILE,
    B_RETURN,
    B_NONE
} BuiltinClass;

typedef struct {
    ExprClass class;
    void * expr;
} Expr;

typedef struct { u32 localsb; Expr body; } Function;

typedef struct { Ident ident; } OpVar;
typedef struct { Ident ident; Expr val; } OpAssign;

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

static char * builtin_names[] = {
    [B_ADD] = "+",
    [B_SUBTRACT] = "-",
    [B_MULTIPLY] = "*",
    [B_DIVIDE] = "/",
    [B_IF] = "if",
    [B_WHILE] = "while",
    [B_RETURN] = "return"
};

