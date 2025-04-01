#include "eval.h"


/*
 * "the stack" is a data stack only. It's not actually a call stack.
 *
 *
 *   global n        <--- stack + STACK_SIZE
 *                   
 *   ...             
 *   global 2        
 *   global 1        <--- gp
 *   main local n    
 *                   
 *   ...             
 *   main local 2    
 *   main local 1    <--- sp (at program start)
 *   func1 local n   
 *                   
 *   ...             
 *   func1 local 2   
 *   func1 local 1
 *   func2 local n
 *
 *   ...             
 *   func2 local 2   
 *   func2 local 1   <--- sp (now)
 *
 *   ...
 *
 *   ...             <--- stack
 */

/* ever wondered what happens to your function
 * returns when you cast them to (void)? */
char DISCARD_BUF[TYPE_MAX_SIZE];
void * const DISCARD = DISCARD_BUF;

typedef struct {
    void *gp, *sp;
} Registers;

static void eval_expr(Expr expr, Registers * regs, void * val);

#define do_2op(type, op) do { \
    ASSERT(param_count == 2); \
    type lhs, rhs; \
    eval_expr(params[0], regs, &lhs); \
    eval_expr(params[1], regs, &rhs); \
    *(type *)val = lhs op rhs; \
} while(0)


#define do_vop(type, op) do { \
    type acc = 0; \
    for (u32 i = 0; i < param_count; i++) { \
        type new; \
        eval_expr(params[i], regs, &new); \
        acc = acc op new; \
    } \
    *(type *)val = acc; \
} while(0)


static inline void * get_var_ptr(Ident addr, void * gp, void * sp) {
    switch (var_location(addr)) {
        case GLOBAL: return gp + var_offset(addr);
        case LOCAL: return sp + var_offset(addr);
        default: PANIC();
    }
}


static inline void eval_builtin(OpBuiltin * builtin, Registers * regs, void * val) {
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
            {
                Integer i;
                eval_expr(params[0], regs, &i);
                printf("%d\n", i);
                break;
            }
        case B_PRINT_S:
            {
                String s;
                eval_expr(params[0], regs, &s);
                for (u32 i = 0; i < s.len; i++)
                    putchar(s.chars[i]);
                putchar('\n');
            }
            break; 
            
        default:PANIC();
    }

}

static inline void eval_if(OpIf * op, Registers * regs, void * val) {
    Integer condition;
    eval_expr(op->cond, regs, &condition);
    if (condition) eval_expr(op->if_expr, regs, DISCARD);
}


static inline void eval_if_else(
    OpIfElse * op, Registers * regs, void * val
) {
    Integer condition;
    eval_expr(op->cond, regs, &condition);
    if (condition) eval_expr(op->if_expr, regs, val);
    else eval_expr(op->else_expr, regs, val);
}


static inline void eval_while(OpWhile * op, Registers * regs) {
    Integer condition;
    for(;;) {
        eval_expr(op->cond, regs, &condition);
        if (!condition) break;
        eval_expr(op->while_expr, regs, DISCARD);
    }
}


static inline void eval_seq(OpSeq * op, Registers * regs, void * val) {
    if (!op->count) return;
    
    for (u32 i = 0; i < op->count - 1; i++) {
        eval_expr(op->exprs[i], regs, DISCARD);
    }

    eval_expr(op->exprs[op->count - 1], regs, val);
}


static inline void eval_var(OpVar * var, Registers * regs, void * val) {
    void * var_ptr = get_var_ptr(var->ident, regs->gp, regs->sp);
    memcpy(val, var_ptr, var->size); 
}


static inline void eval_assign(
    OpAssign * op, Registers * regs, void * val
) {
    void * var_ptr = get_var_ptr(op->ident, regs->gp, regs->sp);
    eval_expr(op->val, regs, var_ptr);
    memcpy(val, var_ptr, op->size); 
}


static inline void eval_call(
    OpCall * call, Registers * regs, void * val
) {
    Function * function;
    eval_expr(call->fn, regs, &function);
    
    void * new_sp = regs->sp - function->stack_size;

    for (u32 i = 0; i < call->param_count; i++) {
        void * ptr = new_sp + call->param_offsets[i];
        eval_expr(call->params[i], regs, ptr);
    }

    regs->sp = new_sp;

    eval_expr(function->body, regs, val);

    regs->sp += function->stack_size;
}


static void eval_expr(
    Expr expr, Registers * regs, void * val
) {
    switch (expr.class) {
        case OP_VAR:
            eval_var(expr.expr, regs, val);
            break;
        case OP_ASSIGN:
            eval_assign(expr.expr, regs, val);
            break;
        case OP_CALL:
            eval_call(expr.expr, regs, val);
            break;
        case OP_BUILTIN:
            eval_builtin(expr.expr, regs, val);
            break; 
        case OP_IF:
            eval_if(expr.expr, regs, val);
            break;
        case OP_IF_ELSE:
            eval_if_else(expr.expr, regs, val);
            break;
        case OP_WHILE:
            eval_while(expr.expr, regs);
            break;
        case OP_SEQ:
            eval_seq(expr.expr, regs, val);
            break;
        case INT_LITERAL:
            *(Integer *) val = *(Integer *) expr.expr;
            break;
        case STR_LITERAL:
            *(String *) val = *(String *) expr.expr;
            break;
        case FN_LITERAL:
            *(Function **) val = (Function *) expr.expr;
            break;
        default:
            printf("%d\n", expr.class);
            PANIC();
    }
}


int eval_ast(AST ast, u32 stack_size) {

    void *stack = malloc(stack_size);
    if (!stack) PANIC("failed to allocate stack");


    Registers regs = {
        .gp = stack + stack_size - ast.globals_size,
        .sp = NULL
    };

    for (u32 i = 0; i < ast.global_count; i++)
        eval_expr(ast.global_decls[i], &regs, DISCARD);


    Function *main_fn = *(Function **) get_var_ptr(ast.main, regs.gp, NULL);

    regs.sp = regs.gp - main_fn->stack_size;

    void *ret_ptr; 
    Integer exit_code = 0;

    if (ast.main_returns_exit_code)
        ret_ptr = &exit_code;
    else ret_ptr = DISCARD;
    
    eval_expr(main_fn->body, &regs, ret_ptr);

    free(stack);

    return exit_code;
}
