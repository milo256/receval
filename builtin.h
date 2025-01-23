#include "types.h"

#include<string.h>
/* compiler is too dumb to realize we do use these */
#include<stdlib.h>
#include<stdio.h>

static char * builtin_names[] = {
    [B_ADD_I] = "+",
    [B_SUB_I] = "-",
    [B_MUL_I] = "*",
    [B_DIV_I] = "/",
    [B_IF] = "if",
    [B_WHILE] = "while",
    [B_SEQ] = "seq",
    [B_PRINT_I] = "print"
};

void eval_expr(Expr *, void *, void *, void *, void *, u32);

void eval_builtin(
    OpBuiltin * builtin, void * globals, void * locals, void * outer_args, void * ret_ptr, u32 retb
) {
    Expr * args = builtin->args;
    u32 args_len = builtin->args_len;
    u32 * arg_typesb = builtin->arg_typesb;

    Integer condition;
    Integer val_tmp;
    switch (builtin->class) {
        case B_ADD_I: case B_SUB_I: case B_MUL_I: case B_DIV_I: 
            ASSERT(args_len > 1);
            ASSERT(retb == 0 || retb == sizeof(Integer));
            if (args_len == 2) {
                Integer lhs, rhs;
                eval_expr(args, globals, locals, outer_args, &lhs, sizeof(Integer));
                eval_expr(&args[1], globals, locals, outer_args, &rhs, sizeof(Integer));
                if (retb) switch (builtin->class) {
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
            ASSERT(arg_typesb[0] == sizeof(Integer));

            eval_expr(args, globals, locals, outer_args, &condition, sizeof(Integer));
            if (condition)
                eval_expr(&args[1], globals, locals, outer_args, ret_ptr, retb);
            else if (args_len > 2)
                eval_expr(&args[2], globals, locals, outer_args, ret_ptr, retb);
            else
                memset(ret_ptr, 0, retb);
            break;
        case B_WHILE:
            ASSERT(args_len == 2);
            ASSERT(arg_typesb[0] == sizeof(Integer));
            
            eval_expr(args, globals, locals, outer_args, &condition, sizeof(Integer));
            while (condition) {
                eval_expr(&args[1], globals, locals, outer_args, ret_ptr, retb);
                eval_expr(args, globals, locals, outer_args, &condition, sizeof(Integer));
            }
            break;
        case B_SEQ:
            ASSERT(args_len > 0);
            for (u32 i = 0; i < args_len - 1; i++) {
                eval_expr(&args[i], globals, locals, outer_args, ret_ptr, 0);
            }
            eval_expr(&args[args_len-1], globals, locals, outer_args, ret_ptr, retb);
            break;
        case B_PRINT_I:
            eval_expr(args, globals, locals, outer_args, &val_tmp, sizeof(Integer));
            printf("%d\n", val_tmp);
            if (retb)
                memcpy(ret_ptr, &val_tmp, retb);
            break;
            
        default:PANIC();
    }

}
