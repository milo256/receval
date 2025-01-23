#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include<string.h>

#include "types.h"
#include "parse.h"

/* my z key is broken. sof = sizeof and b at the end of a
 * variable name means it's a size in (b)ytes */
#define sof sizeof

void eval_function(Function *, void *, void *, void *, u32);

/* args is args of containing function. effectively more locals */
void eval_expr(
    Expr * expr, void * globals, void * locals,
    void * args, void * ret_ptr, u32 retb
) {
    Ident var;
    OpCall * call;
    OpBuiltin * builtin;
    Literal * lit;
    switch (expr->class) {
        case OP_VAR: case OP_ASSIGN:
            var = OP_VAR ? ((OpVar *) expr->expr)->ident : ((OpAssign *) expr->expr)->ident;
            void * loc_ptr[] = { [GLOBAL] = globals, [LOCAL] = locals, [ARGUMENT] = args };
            void * var_ptr = loc_ptr[var_location(var)] + var_offset(var);
            if (expr->class == OP_VAR) {
                memcpy(ret_ptr, var_ptr, retb); 
            } else {
                OpAssign * op = (OpAssign *) expr->expr;
                eval_expr(
                    &op->val, globals, locals, args,
                    var_ptr, op->size
                );
                memcpy(ret_ptr, var_ptr, retb); 
            }
            break;
        case OP_CALL:
            call = (OpCall *) expr->expr;
            void * arg_vals = malloc(call->argsb);
            void * arg_ptr = arg_vals;

            Function * fn;
            eval_expr(&call->fn, globals, locals, args, &fn, sof_type[TYPE_FN_PTR]);

            for (u32 i = 0; i < call->args_len; i++) {
                eval_expr(
                    &call->args[i], globals, locals, args,
                    arg_ptr, call->arg_typesb[i]
                );
                arg_ptr += call->arg_typesb[i];
            }
            eval_function(fn, globals, arg_vals, ret_ptr, retb);
            free(arg_vals);
            break;
        case OP_BUILTIN:
            builtin = (OpBuiltin *) expr->expr;

            eval_builtin(builtin, globals, locals, args, ret_ptr, retb);
            break; 
        case LITERAL:
            lit = (Literal *) expr->expr;
            memcpy(ret_ptr, lit->val, retb);
            break;
        default:
            PANIC();
    }
}

void eval_function(Function * fn, void * globals, void * args, void * ret_ptr, u32 retb) {
    void * locals = malloc(fn->localsb);

    eval_expr(&fn->body, globals, locals, args, ret_ptr, retb);

    free(locals); /* need to free each local individually when reference types are added */
}

int eval_main(Function * fn, void * globals) {
    void * ret_ptr = malloc(sof_type[TYPE_INT]);
    eval_function(fn, globals, NULL, ret_ptr, sof_type[TYPE_INT]);

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
    Token * tokens = tokenize(code);
    //print_tokens(tokens);
    parse_tokens(&tokens, &globals, &main_fn);
    int ret = eval_main(main_fn, globals);
    printf("%d\n",ret);
}
