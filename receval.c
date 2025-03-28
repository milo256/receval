#include "common.h"
#include "parser.h"
#include "ident.h"


/* my z key is broken. sof = sizeof and b at the end of a
 * variable name means it's a size in (b)ytes */
#define sof sizeof


/* ever wondered what happens to your function returns
 * when you cast them to (void)? */
const char DISCARD[TYPE_MAX_SIZE];


void eval_function(Function *, void *, void *, void *);

void eval_expr(Expr *, void *, void *, void *);


void eval_builtin(
    OpBuiltin * builtin, void * globals, void * locals, void * ret_ptr
) {
    Expr * args = builtin->args;
    u32 args_len = builtin->args_len;

    Integer condition;
    switch (builtin->class) {
        case B_ADD_I: case B_SUB_I: case B_MUL_I: case B_DIV_I: 
            ASSERT(args_len > 1);
            if (args_len == 2) {
                Integer lhs, rhs;
                eval_expr(args, globals, locals, &lhs);
                eval_expr(&args[1], globals, locals, &rhs);
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

            eval_expr(args, globals, locals, &condition);
            if (condition)
                eval_expr(&args[1], globals, locals, ret_ptr);
            else if (args_len > 2)
                eval_expr(&args[2], globals, locals, ret_ptr);
            else
                memset(ret_ptr, 0, sizeof(Integer));
            break;
        case B_WHILE:
            ASSERT(args_len == 2);
            
            eval_expr(args, globals, locals, &condition);
            while (condition) {
                eval_expr(&args[1], globals, locals, ret_ptr);
                eval_expr(args, globals, locals, &condition);
            }
            break;
        case B_SEQ:
            ASSERT(args_len > 0);
            for (u32 i = 0; i < args_len - 1; i++) {
                eval_expr(&args[i], globals, locals, ret_ptr);
            }
            eval_expr(&args[args_len-1], globals, locals, ret_ptr);
            break;
        case B_PRINT_I:
            eval_expr(args, globals, locals, ret_ptr);
            printf("%d\n", *((Integer *) ret_ptr));
            break;
            
        default:PANIC();
    }

}


void eval_expr(
    Expr * expr, void * globals, void * locals, void * ret_ptr
) {
    Ident var;
    OpCall * call;
    OpBuiltin * builtin;
    Literal * lit;
    u32 ret_size = sof_type[expr->ret_class];
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
                u32 arg_size = sof_type[call->args[i].ret_class];
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
            builtin = (OpBuiltin *) expr->expr;

            eval_builtin(builtin, globals, locals, ret_ptr);
            break; 
        case LITERAL:
            lit = (Literal *) expr->expr;
            memcpy(ret_ptr, lit->val, ret_size);
            break;
        default:
            PANIC();
    }
}


int eval_main(Function * fn, void * globals) {
    void * ret_ptr = malloc(sof_type[TYPE_INT]);

    void * locals = malloc(fn->stack_size);

    eval_expr(&fn->body, globals, locals, ret_ptr);
    
    free(locals);
    int ret = *(int *)ret_ptr;
    free(ret_ptr);
    return ret;
}


int main(int argc, char * argv[]) {
    char * filename = argc > 1 ? argv[1] : "demo.re";
    FILE * f = fopen(filename, "r");

    if (!f) PANIC();

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
