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
typedef i32 Integer;
typedef u32 Ident;


typedef struct {
    char * chars;
    u32 len;
} LStr;



/* Receval Types
 *------------------------------------------------------------------------------
 */

/* TypeClass defines the type's size on the stack but does not
 * include the full type information. see parse.c */
typedef enum {
    TYPE_NONE = 0,
    TYPE_FN_PTR,
    TYPE_INT,
    TYPE_STR,
} TypeClass;


#define TYPE_MAX_SIZE sizeof(void *)

static u32 sof_type[] = {
    [TYPE_INT] = sizeof(Integer),
    [TYPE_FN_PTR] = sizeof(void *)
};



/* Receval Expressions
 *------------------------------------------------------------------------------
 */

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
    TypeClass ret_class;
    void * expr;
} Expr;


typedef struct { u32 stack_size; Expr body; } Function;

typedef struct { Ident ident; } OpVar;

typedef struct { Ident ident; Expr val; } OpAssign;

typedef struct { void * val; } Literal;


typedef struct {
    BuiltinClass class;
    Expr * args;
    u32 args_len;
} OpBuiltin;


typedef struct {
    Expr fn;
    Expr * args;
    u32 args_len;
} OpCall;



/* Common Macros
 *------------------------------------------------------------------------------
 */

#define SATISFY_COMPILER 69420

#define MAX(A,B) ((A > B) ? A : B)

#define MIN(A,B) ((A > B) ? B : A)

#define ARRLEN(A) (sof(A)/sof(A[0]))


#define ASSERT(A) do{ if (!(A)) {                                                      \
    fprintf(stderr, "Assertion failed on %s:%d: %s = %d\n", __FILE__, __LINE__, #A, A);\
    exit(1);                                                                           \
}} while(0)


#define PANIC(...) do {                                  \
    fprintf(stderr, "Panic! %s:%d\n", __FILE__, __LINE__); \
    exit(-1);                                            \
} while(0)



/* String Functions
 *------------------------------------------------------------------------------
 */

#define LSTR(cstr) (LStr) { .chars = cstr, .len = strlen(cstr) }

int lstr_eq(LStr a, LStr b);

