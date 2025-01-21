
#include "types.h"
#include "macros.h"

#include<string.h>
/* compiler is too dumb to realize we do use these */
#include<stdlib.h>
#include<stdio.h>

void eval_expr(Expr *, void *, void *, void *, void *, u32);

void eval_builtin(
    OpBuiltin * builtin, void * globals, void * locals, void * outer_args, void * ret_ptr, u32 retb
) {
    Expr * args = builtin->args;
    u32 args_len = builtin->args_len;
    u32 * arg_typesb = builtin->arg_typesb;

    Integer condition;
    switch (builtin->class) {
        case B_ADD:
        case B_SUBTRACT:
        case B_MULTIPLY:
        case B_DIVIDE: 
            PANIC();
            break;
        case B_IF:
            ASSERT(args_len > 1 && args_len < 4);
            ASSERT(arg_typesb[0] == sizeof(Integer));

            eval_expr(args, globals, locals, outer_args, &condition, sizeof(Integer));
            if (condition)
                eval_expr(&args[1], globals, locals, outer_args, ret_ptr, retb);
            else if (args_len > 1)
                eval_expr(&args[2], globals, locals, outer_args, ret_ptr, retb);
            else
                memset(ret_ptr, 0, retb);
            break;
        case B_WHILE:
            break;
        case B_RETURN:
            break;
        default:PANIC();
    }

}
