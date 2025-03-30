#include "eval.h"


/* ever wondered what happens to your function
 * returns when you cast them to (void)? */
char DISCARD_BUF[TYPE_MAX_SIZE];
void * const DISCARD = DISCARD_BUF;

static void eval_expr(Expr, void *, void *, void *);

#define seval_expr(expr, ret_ptr) eval_expr((expr), globals, locals, (ret_ptr))


#define do_2op(type, op) do { \
    ASSERT(param_count == 2); \
    type lhs, rhs; \
    seval_expr(params[0], &lhs); \
    seval_expr(params[1], &rhs); \
    *(type *)ret_ptr = lhs op rhs; \
} while(0)


#define do_vop(type, op) do { \
    type acc = 0; \
    for (u32 i = 0; i < param_count; i++) { \
        type new; \
        seval_expr(params[i], &new); \
        acc = acc op new; \
    } \
    *(type *)ret_ptr = acc; \
} while(0)


static void eval_builtin(
    OpBuiltin * builtin, void * globals, void * locals, void * ret_ptr
) {
    Expr * params = builtin->args;
    u32 param_count = builtin->args_len;

    switch (builtin->class) {
        case B_ADD_I: 
            do_2op(Integer, +);
            break;
        case B_SUB_I:
            do_2op(Integer, -);
            break;
        case B_MUL_I:
            do_2op(Integer, *);
            break;
        case B_DIV_I: 
            do_2op(Integer, /);
            break;
        case B_ADD_VI:
            do_vop(Integer, +);
            break;
        case B_MUL_VI:
            do_vop(Integer, *);
            break;
        case B_PRINT_I:
            eval_expr(params[0], globals, locals, ret_ptr);
            printf("%d\n", *((Integer *) ret_ptr));
            break;

        case B_PRINT_S:
            {
                String str;
                eval_expr(params[0], globals, locals, &str);
                for (u32 i = 0; i < str.len; i++)
                    putchar(str.chars[i]);
                putchar('\n');
            }
            break; 
            
        default:PANIC();
    }

}


static void eval_if(OpIf * op, void * globals, void * locals) {
    Integer condition;
    seval_expr(op->cond, &condition);
    if (condition) seval_expr(op->if_expr, DISCARD);
}


static void eval_if_else(
    OpIfElse * op, void * globals, void * locals, void * ret_ptr
) {
    Integer condition;
    seval_expr(op->cond, &condition);
    if (condition) seval_expr(op->if_expr, ret_ptr);
    else seval_expr(op->else_expr, ret_ptr);
}


static void eval_while(OpWhile * op, void * globals, void * locals) {
    Integer condition;
    for(;;) {
        seval_expr(op->cond, &condition);
        if (!condition) break;
        seval_expr(op->while_expr, DISCARD);
    }
}


static void eval_seq(OpSeq * op, void * globals, void * locals, void * ret_ptr) {
    if (!op->count) return;
    
    for (u32 i = 0; i < op->count - 1; i++) {
        eval_expr(op->exprs[i], globals, locals, DISCARD);
    }

    eval_expr(op->exprs[op->count - 1], globals, locals, ret_ptr);
}


static void * get_var_ptr(Ident ident, void * globals, void * locals) {
    switch (var_location(ident)) {
        case GLOBAL: return globals + var_offset(ident);
        case LOCAL: return locals + var_offset(ident);
        default: PANIC();
    }
}


static void eval_var(
    OpVar * var, void * globals, void * locals, void * ret_ptr
) {
    void * var_ptr = get_var_ptr(var->ident, globals, locals);
    memcpy(ret_ptr, var_ptr, var->size); 
}


static void eval_assign(
    OpAssign * op, void * globals, void * locals, void * ret_ptr
) {
    void * var_ptr = get_var_ptr(op->ident, globals, locals);
    eval_expr(op->val, globals, locals, var_ptr);
    memcpy(ret_ptr, var_ptr, op->size); 
}


static void eval_call(OpCall * call, void * gp, void * sp, void * ret_ptr) {
    Function * function;
    eval_expr(call->fn, gp, sp, &function);
    
    void * new_sp = sp - function->stack_size;

    for (u32 i = 0; i < call->param_count; i++) {
        void * ptr = new_sp + call->param_offsets[i];
        eval_expr(call->params[i], gp, sp, ptr);
    }
    
    eval_expr(function->body, gp, new_sp, ret_ptr);
}


static void eval_expr(
    Expr expr, void * gp, void * sp, void * ret_ptr
) {
    switch (expr.class) {
        case OP_VAR:
            eval_var(expr.expr, gp, sp, ret_ptr);
            break;
        case OP_ASSIGN:
            eval_assign(expr.expr, gp, sp, ret_ptr);
            break;
        case OP_CALL:
            eval_call(expr.expr, gp, sp, ret_ptr);
            break;
        case OP_BUILTIN:
            eval_builtin(expr.expr, gp, sp, ret_ptr);
            break; 
        case OP_IF:
            eval_if(expr.expr, gp, sp);
            break;
        case OP_IF_ELSE:
            eval_if_else(expr.expr, gp, sp, ret_ptr);
            break;
        case OP_WHILE:
            eval_while(expr.expr, gp, sp);
            break;
        case OP_SEQ:
            eval_seq(expr.expr, gp, sp, ret_ptr);
            break;
        case INT_LITERAL:
            *(Integer *) ret_ptr = *(Integer *) expr.expr;
            break;
        case STR_LITERAL:
            *(String *) ret_ptr = *(String *) expr.expr;
            break;
        case FN_LITERAL:
            *(Function **) ret_ptr = (Function *) expr.expr;
            break;
        default:
            printf("%d\n", expr.class);
            PANIC();
    }
}


int eval_ast(AST ast, u32 stack_size) {

    void *sp, * gp, * stack = malloc(stack_size);
    if (!stack) PANIC("failed to allocate stack");

    gp = stack + stack_size - ast.globals_size;

    for (u32 i = 0; i < ast.global_count; i++)
        eval_expr(ast.global_decls[i], gp, NULL, DISCARD);


    Function * main_fn = *(Function **) get_var_ptr(ast.main, gp, NULL);

    sp = gp - main_fn->stack_size;


    void * ret_ptr; 
    Integer exit_code = 0;

    if (ast.main_returns_exit_code)
        ret_ptr = &exit_code;
    else ret_ptr = DISCARD;

    eval_expr(main_fn->body, gp, sp, ret_ptr);

    free(stack);

    return exit_code;
}

