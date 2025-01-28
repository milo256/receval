#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include<string.h>

#include "types.h"
#include "parse.h"

/* my z key is broken. sof = sizeof and b at the end of a
 * variable name means it's a size in (b)ytes */
#define sof sizeof

/* ever wondered what happens to your function returns when
 * you cast them to (void)? */
const char DISCARD[TYPE_MAX_SIZE];

void eval_function(Function *, void *, void *, void *);
void eval_expr(Expr *, void *, void *, void *, void *);

void eval_builtin(
    OpBuiltin * builtin, void * globals, void * locals, void * outer_args, void * ret_ptr
) {
    Expr * args = builtin->args;
    u32 args_len = builtin->args_len;

    Integer condition;
    switch (builtin->class) {
        case B_ADD_I: case B_SUB_I: case B_MUL_I: case B_DIV_I: 
            ASSERT(args_len > 1);
            if (args_len == 2) {
                Integer lhs, rhs;
                eval_expr(args, globals, locals, outer_args, &lhs);
                eval_expr(&args[1], globals, locals, outer_args, &rhs);
                switch (builtin->class) {
                    case B_ADD_I: *(Integer *)ret_ptr = lhs + rhs; break;
                    case B_SUB_I: *(Integer *)ret_ptr = lhs - rhs; break;
                    case B_MUL_I: *(Integer *)ret_ptr = lhs * rhs; break;
                    case B_DIV_I: *(Integer *)ret_ptr = lhs / rhs; break;
                    default: PANIC();
                }
            } else PANIC();
            break;
        case B_IF:
            ASSERT(args_len > 1 && args_len < 4);

            eval_expr(args, globals, locals, outer_args, &condition);
            if (condition)
                eval_expr(&args[1], globals, locals, outer_args, ret_ptr);
            else if (args_len > 2)
                eval_expr(&args[2], globals, locals, outer_args, ret_ptr);
            else
                memset(ret_ptr, 0, sizeof(Integer));
            break;
        case B_WHILE:
            ASSERT(args_len == 2);
            
            eval_expr(args, globals, locals, outer_args, &condition);
            while (condition) {
                eval_expr(&args[1], globals, locals, outer_args, ret_ptr);
                eval_expr(args, globals, locals, outer_args, &condition);
            }
            break;
        case B_SEQ:
            ASSERT(args_len > 0);
            for (u32 i = 0; i < args_len - 1; i++) {
                eval_expr(&args[i], globals, locals, outer_args, ret_ptr);
            }
            eval_expr(&args[args_len-1], globals, locals, outer_args, ret_ptr);
            break;
        case B_PRINT_I:
            eval_expr(args, globals, locals, outer_args, ret_ptr);
            printf("%d\n", *((Integer *) ret_ptr));
            break;
            
        default:PANIC();
    }

}

/* args is args of containing function. effectively more locals */
void eval_expr(
    Expr * expr, void * globals, void * locals,
    void * args, void * ret_ptr
) {
    Ident var;
    OpCall * call;
    OpBuiltin * builtin;
    Literal * lit;
    u32 ret_size = sof_type[expr->ret_class];
    switch (expr->class) {
        case OP_VAR: case OP_ASSIGN:
            var = OP_VAR ? ((OpVar *) expr->expr)->ident : ((OpAssign *) expr->expr)->ident;
            void * loc_ptr[] = { [GLOBAL] = globals, [LOCAL] = locals, [ARGUMENT] = args };
            void * var_ptr = loc_ptr[var_location(var)] + var_offset(var);
            if (expr->class == OP_VAR) {
                memcpy(ret_ptr, var_ptr, ret_size); 
            } else {
                OpAssign * op = (OpAssign *) expr->expr;
                eval_expr(
                    &op->val, globals, locals, args,
                    var_ptr
                );
                memcpy(ret_ptr, var_ptr, ret_size); 
            }
            break;
        case OP_CALL:
            call = (OpCall *) expr->expr;
            void * arg_vals = malloc(call->argsb);
            void * arg_ptr = arg_vals;

            Function * fn;
            eval_expr(&call->fn, globals, locals, args, &fn);

            for (u32 i = 0; i < call->args_len; i++) {
                u32 arg_size = sof_type[call->args[i].ret_class];
                eval_expr(
                    &call->args[i], globals, locals, args,
                    arg_ptr
                );
                arg_ptr += arg_size;
            }
            eval_function(fn, globals, arg_vals, ret_ptr);
            free(arg_vals);
            break;
        case OP_BUILTIN:
            builtin = (OpBuiltin *) expr->expr;

            eval_builtin(builtin, globals, locals, args, ret_ptr);
            break; 
        case LITERAL:
            lit = (Literal *) expr->expr;
            memcpy(ret_ptr, lit->val, ret_size);
            break;
        default:
            PANIC();
    }
}

void eval_function(Function * fn, void * globals, void * args, void * ret_ptr) {
    void * locals = malloc(fn->localsb);

    eval_expr(&fn->body, globals, locals, args, ret_ptr);

    free(locals); /* need to free each local individually when reference types are added */
}

int eval_main(Function * fn, void * globals) {
    void * ret_ptr = malloc(sof_type[TYPE_INT]);
    eval_function(fn, globals, NULL, ret_ptr);

    int ret = *(int *)ret_ptr;
    free(ret_ptr);
    return ret;
}

#define tk(class) (Token) { .class = class, .str = NULL, .strlen = 0 }

int main(int argc, char * argv[]) {
    char * filename = argc > 1 ? argv[1] : "demo.re";
    FILE * f = fopen(filename, "r");

    if (!f)
        PANIC();

    fseek(f, 0, SEEK_END);
    u32 code_len = ftell(f);
    rewind(f);
    char * code = malloc(code_len);
    fread(code, 1, code_len, f); 
    fclose(f);

    void * globals;
    Function * main_fn;
    parse_code(code, &globals, &main_fn);
    int ret = eval_main(main_fn, globals);
    printf("%d\n",ret);
}
