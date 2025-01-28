#include "types.h"

#include<string.h>
/* compiler is too dumb to realize we do use these */
#include<stdlib.h>
#include<stdio.h>

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
