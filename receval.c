#include "common.h"
#include "parser.h"
#include "ident.h"


/* my z key is broken. sof = sizeof and b at the end of a
 * variable name means it's a size in (b)ytes */
#define sof sizeof


/* ever wondered what happens to your function returns
 * when you cast them to (void)? */
char DISCARD_BUF[TYPE_MAX_SIZE];
void * const DISCARD = DISCARD_BUF;


void eval_function(Function *, void *, void *, void *);

void eval_expr(Expr *, void *, void *, void *);

#define seval_expr(expr, ret_ptr) eval_expr((expr), globals, locals, (ret_ptr))

#define do_2op(type, op) do { \
    ASSERT(param_count == 2); \
    type lhs, rhs; \
    seval_expr(params, &lhs); \
    seval_expr(params + 1, &rhs); \
    *(type *)ret_ptr = lhs op rhs; \
} while(0)

#define do_vop(type, op) do { \
    type acc = 0; \
    for (u32 i = 0; i < param_count; i++) { \
        type new; \
        seval_expr(params + i, &new); \
        acc = acc op new; \
    } \
    *(type *)ret_ptr = acc; \
} while(0)

void eval_builtin(
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
        case B_SEQ:
            ASSERT(param_count > 0);
            for (u32 i = 0; i < param_count - 1; i++) {
                eval_expr(&params[i], globals, locals, ret_ptr);
            }
            eval_expr(&params[param_count-1], globals, locals, ret_ptr);
            break;
        case B_PRINT_I:
            eval_expr(params, globals, locals, ret_ptr);
            printf("%d\n", *((Integer *) ret_ptr));
            break;

        case B_PRINT_S:
            {
                String str;
                eval_expr(params, globals, locals, &str);
                for (u32 i = 0; i < str.len; i++)
                    putchar(str.chars[i]);
                putchar('\n');
            }
            break; 
            
        default:PANIC();
    }

}


void eval_if(OpIf * op, void * globals, void * locals) {
    Integer condition;
    seval_expr(&op->cond, &condition);
    if (condition) seval_expr(&op->if_expr, DISCARD);
}


void eval_if_else(OpIfElse * op, void * globals, void * locals, void * ret_ptr) {
    Integer condition;
    seval_expr(&op->cond, &condition);
    if (condition) seval_expr(&op->if_expr, ret_ptr);
    else seval_expr(&op->else_expr, ret_ptr);
}


void eval_while(OpWhile * op, void * globals, void * locals) {
    Integer condition;
    for(;;) {
        seval_expr(&op->cond, &condition);
        if (!condition) break;
        seval_expr(&op->while_expr, DISCARD);
    }
}


void eval_seq(OpSeq * op, void * globals, void * locals, void * ret_ptr) {
    if (!op->count) return;
    
    for (u32 i = 0; i < op->count - 1; i++) {
        eval_expr(op->exprs + i, globals, locals, DISCARD);
    }

    eval_expr(op->exprs + op->count - 1, globals, locals, ret_ptr);
}


void eval_expr(
    Expr * expr, void * globals, void * locals, void * ret_ptr
) {
    Ident var;
    OpCall * call;
    u32 ret_size = expr->ret_size;
    switch (expr->class) {
        case OP_VAR: case OP_ASSIGN:
            var = OP_VAR ? ((OpVar *) expr->expr)->ident : ((OpAssign *) expr->expr)->ident;
            void * loc_ptr[] = { [GLOBAL] = globals, [LOCAL] = locals };
            void * var_ptr = loc_ptr[var_location(var)] + var_offset(var);
            if (expr->class == OP_VAR) {
                memcpy(ret_ptr, var_ptr, ret_size); 
            } else {
                OpAssign * op = (OpAssign *) expr->expr;
                eval_expr(
                    &op->val, globals, locals,
                    var_ptr
                );
                memcpy(ret_ptr, var_ptr, ret_size); 
            }
            break;
        case OP_CALL:
            call = (OpCall *) expr->expr;
            Function * fn;
            eval_expr(&call->fn, globals, locals, &fn);
            
            void * new_locals = malloc(fn->stack_size);
            void * arg_ptr = new_locals;

            for (u32 i = 0; i < call->args_len; i++) {
                u32 arg_size = call->args[i].ret_size;
                eval_expr(
                    &call->args[i], globals, locals,
                    arg_ptr
                );
                arg_ptr += arg_size;
            }
            eval_expr(&fn->body, globals, new_locals, ret_ptr);
            free(new_locals);
            break;
        case OP_BUILTIN:
            eval_builtin(expr->expr, globals, locals, ret_ptr);
            break; 
        case OP_IF:
            eval_if(expr->expr, globals, locals);
            break;
        case OP_IF_ELSE:
            eval_if_else(expr->expr, globals, locals, ret_ptr);
            break;
        case OP_WHILE:
            eval_while(expr->expr, globals, locals);
            break;
        case OP_SEQ:
            eval_seq(expr->expr, globals, locals, ret_ptr);
            break;
        case INT_LITERAL:
            *(Integer *) ret_ptr = *(Integer *) expr->expr;
            break;
        case STR_LITERAL:
            *(String *) ret_ptr = *(String *) expr->expr;
            break;
        case FN_LITERAL:
            *(Function *) ret_ptr = *(Function *) expr->expr;
            break;
        default:
            printf("%d\n", expr->class);
            PANIC();
    }
}


int eval_main(Function * fn, void * globals) {
    void * ret_ptr = malloc(sizeof(Integer));

    void * locals = malloc(fn->stack_size);

    eval_expr(&fn->body, globals, locals, ret_ptr);
    
    free(locals);
    int ret = *(int *)ret_ptr;
    free(ret_ptr);
    return ret;
}


int main(int argc, char * argv[]) {
    if (argc < 2) return fprintf(stderr, "provide file\n"), -1;
    if (argc > 2) return fprintf(stderr, "too many arguments\n"), -1;

    FILE * f = fopen(argv[1], "r");

    if (!f) return fprintf(stderr, "file doesn't exist\n"), -1;

    fseek(f, 0, SEEK_END);
    u32 code_len = ftell(f);
    rewind(f);
    char * code = malloc(code_len + 1);
    fread(code, 1, code_len, f); 
    code[code_len] = 0;
    fclose(f);

    void * globals;
    Function * main_fn;
    parse_code(code, &globals, &main_fn);
    int ret = eval_main(main_fn, globals);
    printf("%d\n",ret);
    free_code();
}
