#pragma once
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>


typedef uint32_t u32;
typedef int32_t i32;
typedef uint8_t u8;
typedef int8_t i8;

typedef struct {
    char * chars;
    u32 len;
} LStr;

#define TYPE_MAX_SIZE (2 * sizeof(void *))



/* Receval Expressions
 *------------------------------------------------------------------------------
 */

typedef u32 Ident;

typedef enum {
    INT_LITERAL,
    STR_LITERAL,
    FN_LITERAL,
    OP_CALL,
    OP_VAR,
    OP_ASSIGN,
    OP_BUILTIN,
    OP_IF,
    OP_IF_ELSE,
    OP_WHILE,
    OP_SEQ,
} ExprClass;


typedef enum {
    B_NONE,
    B_ADD_I,
    B_SUB_I,
    B_MUL_I,
    B_DIV_I,
    B_ADD_VI,
    B_MUL_VI,
    B_PRINT_I,
    B_PRINT_S
} BuiltinClass;


typedef struct {
    ExprClass class;
    u32 ret_size;
    void * expr;
} Expr;


typedef i32 Integer;

typedef struct { char * chars; u32 len; } String;
typedef struct { u32 stack_size; Expr body; } Function;

typedef struct {
    Expr fn;
    Expr * args;
    u32 args_len;
} OpCall;

typedef struct { Ident ident; } OpVar;
typedef struct { Ident ident; Expr val; } OpAssign;

typedef struct {
    BuiltinClass class;
    Expr * args;
    u32 args_len;
} OpBuiltin;

typedef struct { Expr cond, if_expr; } OpIf;
typedef struct { Expr cond, if_expr, else_expr; } OpIfElse;
typedef struct { Expr cond, while_expr; } OpWhile;
typedef struct { Expr * exprs; u32 count; } OpSeq;



/* Common Macros
 *------------------------------------------------------------------------------
 */

#define SATISFY_COMPILER 69420

#define MAX(A,B) ((A > B) ? A : B)

#define MIN(A,B) ((A > B) ? B : A)

#define ARRLEN(A) (sof(A)/sof(A[0]))

#ifdef NDEBUG
#define ASSERT(A) ((void) 0)
#else
#define ASSERT(A) do{ if (!(A)) {                                                      \
    fprintf(stderr, "Assertion failed on %s:%d: %s\n", __FILE__, __LINE__, #A);\
    exit(-1);                                                                           \
}} while(0)
#endif


#define PANIC(...) do {                                  \
    fprintf(stderr, "Panic! %s:%d\n", __FILE__, __LINE__); \
    exit(-1);                                            \
} while(0)

#define unreachable \
    PANIC("unreachable");



/* String Functions
 *------------------------------------------------------------------------------
 */

#define LSTR(cstr) (LStr) { .chars = cstr, .len = strlen(cstr) }

bool lstr_str_eq(const LStr a, const char * b);
bool lstr_eq(const LStr a, const LStr b);

