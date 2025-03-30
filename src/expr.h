#pragma once
#include "common.h"



/* Receval Expressions
 *------------------------------------------------------------------------------
 */

typedef u32 Ident;

enum VarLocation { GLOBAL, LOCAL };

Ident ident_new(u32 loc, u32 ofs);
u32 var_location(Ident ident);
u32 var_offset(Ident ident);

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
    void * expr;
} Expr;


typedef i32 Integer;

typedef struct { char * chars; u32 len; } String;
typedef struct { u32 stack_size; Expr body; } Function;

typedef struct {
    Expr fn, * params;
    u32 * param_offsets, param_count;
} OpCall;

typedef struct { Ident ident; u32 size; } OpVar;
typedef struct { Ident ident; Expr val; u32 size; } OpAssign;

typedef struct {
    BuiltinClass class;
    Expr * args;
    u32 args_len;
} OpBuiltin;

typedef struct { Expr cond, if_expr; } OpIf;
typedef struct { Expr cond, if_expr, else_expr; } OpIfElse;
typedef struct { Expr cond, while_expr; } OpWhile;
typedef struct { Expr * exprs; u32 count; } OpSeq;

#ifdef urmom
/* It's... kind of an abstract syntax tree
 */
typedef struct {
    Arena arena;
    void * globals;
    Function * main_fn;
    bool ignore_ret; /* main returns void or something else that's not int.
                      * so, ignore it */
} AST;
#endif

typedef struct {
    Arena arena;
    Expr * global_decls;
    u32 globals_size, global_count;
    Ident main;
    bool main_returns_exit_code;
} AST;
