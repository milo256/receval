#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include<string.h>

#include "types.h"
#include "macros.h"
#include "builtin.h"


/* my z key is broken. sof = sizeof and b at the end of a
 * variable name means it's a size in (b)ytes */
#define sof sizeof

bool name_in(char * name, Def * defs, u32 defs_len, u32 * out_index) {
    for (*out_index = 0; *out_index < defs_len; (*out_index)++)
        if (!strcmp(defs[*out_index].name, name)) return true;
    return false;
}

Ident ident_new(u32 loc, u32 ofs) { return (loc << 30) | ofs; }
u32 var_location(Ident ident) { return ((3 << 30) & ident) >> 30; }
u32 var_offset(Ident ident) { return (((1 << 30) - 1) & ident); }

void parse_expr(Token *, Def *, u32, Def *, u32, Def **, u32 *, u32 *, Expr *, Type *, Token **);

void parse_function_call_args(
    Token ** token_ptr, Def * global_defs, u32 globals_len, Def * outer_arg_defs,
    u32 outer_args_len, Def ** local_defs, u32 * locals_len, u32 * locals_cap,
    Expr ** out_args, u32 ** out_arg_typesb, u32 * out_args_len, u32 * out_argsb
) {
    Expr * args;
    u32 args_cap = 2, args_len = 0, argsb = 0, * arg_typesb;

    args = malloc(args_cap * sof(Expr));
    arg_typesb = malloc(args_cap * sof(u32));

    while((**token_ptr).class != TK_CLOSE) {
        if (args_len >= args_cap) {
            args_cap *= 2;
            args = realloc(args, args_cap * sof(Expr));
            arg_typesb = realloc(arg_typesb, args_cap * sof(u32));
        }
        Type arg_type;
        parse_expr(
            *token_ptr, global_defs, globals_len, outer_arg_defs, outer_args_len,
            local_defs, locals_len, locals_cap,
            &args[args_len], &arg_type, token_ptr
        );
        arg_typesb[args_len] = sof_type[arg_type.class];
        argsb += arg_typesb[args_len];
        args_len++;
    }

    *out_args = args;
    *out_arg_typesb = arg_typesb;
    *out_args_len = args_len;
    *out_argsb = argsb;
}

BuiltinClass str_to_builtin(char * str) {
    for (BuiltinClass i = 0; i < ARRLEN(builtin_names); i++) {
        if (!strcmp(str, builtin_names[i]))
            return i;
    }
    return B_NONE;
}

bool parse_var(
    char * var_name,
    Def * global_defs, u32 globals_len, Def * arg_defs,
    u32 args_len, Def * local_defs, u32 locals_len,
    Type * out_type, Ident * out_ident
) {
    u32 var_index;
    if (name_in(var_name, local_defs, locals_len, &var_index)) {
        *out_type = local_defs[var_index].type;
        *out_ident = ident_new(LOCAL, global_defs[var_index].offset);
    } else if (name_in(var_name, arg_defs, args_len, &var_index)) {
        *out_type = arg_defs[var_index].type;
        *out_ident = ident_new(ARGUMENT, arg_defs[var_index].offset);
    } else if (name_in(var_name, global_defs, globals_len, &var_index)) {
        *out_type = global_defs[var_index].type;
        *out_ident = ident_new(GLOBAL, global_defs[var_index].offset);
    } else return false;
    return true;
}

void parse_expr(
    Token * tokens,
    Def * global_defs, u32 globals_len, Def * arg_defs, u32 args_len,
    Def ** local_defs, u32 * locals_len, u32 * locals_cap,
    Expr * out_expr, Type * out_ret_type, Token ** out_next_token
) {
    switch (tokens->class) {
        case TK_INT: {
            Integer integer = atoi(tokens->value);
            *out_ret_type = (Type) { .class = TYPE_INT };
            *out_expr = (Expr) {
                .class = LITERAL,
                .expr = malloc(sof(Literal))
            };
            Literal literal = (Literal) { .val = malloc(sof(Integer)) };
            *(Integer *)(literal.val) = integer;
            *(Literal *)(out_expr->expr) = literal;
            *out_next_token = &tokens[1];
            return;
        } case TK_IDENT: {
            /* ident as expr if not a builtin is a variable, maybe a function call */
            Type var_type;
            Ident ident;

            BuiltinClass bclass = str_to_builtin(tokens->value);

            if (bclass != B_NONE) {
                OpBuiltin op = (OpBuiltin) { .class = bclass };
                Token * token_ptr = &tokens[2];
                parse_function_call_args(
                    &token_ptr,
                    global_defs, globals_len, arg_defs, args_len,
                    local_defs, locals_len, locals_cap,
                    &op.args, &op.arg_typesb, &op.args_len, &op.argsb
                );
                *out_expr = (Expr) { OP_BUILTIN, malloc(sof(OpBuiltin)) };
                *out_ret_type = (Type) { .class = TYPE_INT };
                *(OpBuiltin *)(out_expr->expr) = op;
                *out_next_token = &token_ptr[1];
                return;
            }

            if (!parse_var(
                tokens[0].value,
                global_defs, globals_len, arg_defs, args_len, *local_defs, *locals_len,
                &var_type, &ident
            )) PANIC();

            Expr var_expr = (Expr) { .class = OP_VAR, .expr = malloc(sof(OpVar)) };
            *(OpVar *)(var_expr.expr) = (OpVar) { .ident = ident };

            if (tokens[1].class != TK_OPEN) {
                *out_expr = var_expr;
                *out_ret_type = var_type;
                *out_next_token = &tokens[1];
                return;
            }

            ASSERT(var_type.class == TYPE_FN_PTR);

            OpCall call = (OpCall) { .fn = var_expr };
            
            Token * token_ptr = &tokens[2];
            parse_function_call_args(
                &token_ptr, 
                global_defs, globals_len, arg_defs, args_len,
                local_defs, locals_len, locals_cap,
                &call.args, &call.arg_typesb, &call.args_len, &call.argsb
            );
            
            *out_expr = (Expr) { OP_CALL, malloc(sof(OpCall)) };
            *(OpCall *)(out_expr->expr) = call;

            *out_ret_type = *var_type.ret_type;
            *out_next_token = &token_ptr[1];

            return;
        } case TK_ASSIGN: {
            ASSERT(tokens[1].class == TK_IDENT);

            Expr val;
            Type val_type;
            parse_expr(
                &tokens[2], 
                global_defs, globals_len, arg_defs, args_len,
                local_defs, locals_len, locals_cap,
                &val, &val_type, out_next_token
            );
            
            Type var_type;
            Ident ident;

            if (!parse_var(
                tokens[1].value,
                global_defs, globals_len, arg_defs, args_len, *local_defs, *locals_len,
                &var_type, &ident
            )) {
                var_type = val_type;

                u32 offset = *locals_len ?
                    (*local_defs)[*locals_len - 1].offset + sof_type[(*local_defs)[*locals_len - 1].type.class] : 0;

                ident = ident_new(LOCAL, offset);

                if (*locals_len >= *locals_cap) {
                    *locals_cap *= 2;
                    *local_defs = realloc(*local_defs, *locals_cap * sof(Def));
                }

                (*local_defs)[*locals_len] = (Def) {
                    .name = tokens[1].value,
                    .offset = var_offset(ident),
                    .type = var_type
                };
                (*locals_len)++;
            }
            /* should be deep comparison */
            ASSERT(var_type.class == val_type.class); 

            Expr expr = (Expr) { OP_ASSIGN, malloc(sof(OpAssign)) };
            *(OpAssign*)expr.expr = (OpAssign) {
                .ident = ident,
                .val = val,
                .size = sof_type[var_type.class]
            };
            *out_expr = expr;
            *out_ret_type = var_type;
            return; 
        }
        default: PANIC();
    }
}

Function parse_function_code(
        Token * tokens,
        Def * global_defs, u32 globals_len, Def * arg_defs, u32 args_len,
        Type * out_type
) {
    Function fn;
    Token * end_token;
    Type * ret_type = malloc(sof(Type));

    u32 locals_len = 0, locals_cap = 2;
    Def * local_defs = malloc(sof(Def) * locals_cap);

    parse_expr(
        tokens,
        global_defs, globals_len, arg_defs, args_len,
        &local_defs, &locals_len, &locals_cap,
        &fn.body, ret_type, &end_token
    );

    fn.localsb = locals_len ? local_defs[locals_len - 1].offset : 0;
    if (out_type) (*out_type = (Type) { .class = TYPE_FN_PTR, .ret_type = ret_type });
    ASSERT(end_token[0].class == TK_END);
    return fn;
}

Type str_to_type(char * str) {
    if (!strcmp(str, "int"))
        return (Type) { TYPE_INT };
    else PANIC();
}

void skip_to_end(Token ** token, TkClass begin, TkClass end) {
    ASSERT((*token)->class == begin);
    (*token)++;
    for (u32 d = 1; d && (*token)->class != TK_EOF;) {
        if((*token)->class == begin) d++;
        if((*token)->class == end) d--;
        (*token)++;
    }
}

void parse_function_def(
    Token * token,
    Def ** out_arg_defs, u32 * out_args_len,
    Token ** out_function_code, Token ** out_next_token
) {
    u32 args_len = 0, arg_offset = 0, args_cap = 2;
    Def * arg_defs = malloc(sof(Def) * args_cap);

    ASSERT(token->class == TK_OPEN);
    token++;
    while (token->class != TK_CLOSE) {
        ASSERT(token->class == TK_IDENT);
        ASSERT(token[1].class == TK_IDENT);

        Type type = str_to_type(token->value);
        char * name = token[1].value;

        arg_defs[args_len] = (Def) {
            .name = name,
            .type = type,
            .offset = arg_offset,
            .init = NULL,
        };
        arg_offset += sof_type[type.class];
        args_len++;
        token++; token++;
    }
    *out_arg_defs = arg_defs;
    *out_args_len = args_len;

    ASSERT(token->class == TK_CLOSE); token++;
    ASSERT(token->class == TK_BEGIN);
    
    if (out_function_code) *out_function_code = &token[1];

    skip_to_end(&token, TK_BEGIN, TK_END);

    if (out_next_token) *out_next_token = token;
}

void parse_global_def(Token ** tokens, Def ** global_defs, u32 * globals_len, u32 * globalsb, u32 * globals_cap) {
    Token * token = *tokens;
    ASSERT(token->class == TK_ASSIGN); token++;
    ASSERT(token->class == TK_IDENT);

    char * var_name = token->value;

    Type type;
    
    Token * init_token;
    token++;
    if (token->class == TK_FUNCTION) {
        type = (Type) {
            .class = TYPE_FN_PTR,
            .ret_type = malloc(sof(Type))
        };
        token++;
        ASSERT(token->class == TK_IDENT);
        *(type.ret_type) = str_to_type(token->value);

        token++;
        init_token = token;
        skip_to_end(&token, TK_OPEN, TK_CLOSE);
        skip_to_end(&token, TK_BEGIN, TK_END);
    } else if (token->class == TK_INT) {
        type = (Type) { TYPE_INT };
        init_token = token;
        token++;
    } else PANIC();
    
    if (*globals_len >= *globals_cap) {
        if (*globals_cap == 0) {
            *globals_cap = 2;
            *global_defs = malloc(*globals_cap * sof(Def));
        }
        else {
            *globals_cap *= 2;
            *global_defs = realloc(*global_defs, *globals_cap * sof(Def)); 
        }
    }
    (*global_defs)[*globals_len] = (Def) {
        .name = var_name,
        .offset = *globalsb,
        .type = type,
        .init = init_token
    };
    (*globals_len)++;
    *globalsb += sof_type[type.class];
    *tokens = token;
}

void parse_tokens(Token ** tokens, void ** out_globals, Function ** out_main) {
    u32 globals_len = 0, globalsb = 0, globals_cap = 0;
    Def * global_defs;
    *out_main = NULL;

    while((*tokens)->class != TK_EOF) {
        parse_global_def(tokens, &global_defs, &globals_len, &globalsb, &globals_cap);
    }
    
    void * globals = malloc(globalsb);

    for (u32 i = 0; i < globals_len; i++) {
        Def def = global_defs[i];
        Def * arg_defs;
        u32 args_len;
        Token * function_body;
        switch (def.type.class) {
            case TYPE_FN_PTR:
                parse_function_def(def.init, &arg_defs, &args_len, &function_body, NULL);
                Function * fn = malloc(sof(Function));
                *fn = parse_function_code(function_body, global_defs, globals_len, arg_defs, args_len, NULL); 
                *(Function **)(globals + def.offset) = fn;
                if (!strcmp(def.name,"main")) *out_main = fn;
                break;
            case TYPE_INT:
                ASSERT(((Token *)def.init)->class == TK_INT);
                *(Integer *)(globals + def.offset) = atoi(((Token*)def.init)->value);
                break;
            default: PANIC();
        }
    }
    *out_globals = globals;
}

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
                memcpy(ret_ptr, loc_ptr[var_location(var)] + var_offset(var), retb); 
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

int main() {

    Token *code = (Token[]) {
        (Token) { TK_ASSIGN, NULL },
        (Token) { TK_IDENT, "main" },
            (Token) { TK_FUNCTION, NULL },
            (Token) { TK_IDENT, "int" },
            (Token) { TK_OPEN, NULL },
            (Token) { TK_CLOSE, NULL },
            (Token) { TK_BEGIN, NULL },
                (Token) { TK_IDENT, "seq" },
                (Token) { TK_OPEN, NULL },
                    (Token) { TK_ASSIGN, NULL },
                        (Token) { TK_IDENT, "i" },
                        (Token) { TK_INT, "10" },
                    (Token) { TK_IDENT, "while" },
                    (Token) { TK_OPEN, NULL },
                        (Token) { TK_IDENT, "i" },
                        (Token) { TK_IDENT, "seq" },
                        (Token) { TK_OPEN, NULL },
                            (Token) { TK_ASSIGN, NULL },
                                (Token) { TK_IDENT, "i" },
                                (Token) { TK_IDENT, "-" },
                                (Token) { TK_OPEN, NULL },
                                    (Token) { TK_IDENT, "i" },
                                    (Token) { TK_INT, "1" },
                                (Token) { TK_CLOSE, NULL },
                            (Token) { TK_IDENT, "print" },
                            (Token) { TK_OPEN, NULL },
                                (Token) { TK_IDENT, "i" },
                            (Token) { TK_CLOSE, NULL },
                        (Token) { TK_CLOSE, NULL }, 
                    (Token) { TK_CLOSE, NULL },
                (Token) { TK_CLOSE, NULL },
            (Token) { TK_END, NULL },

        (Token) { TK_EOF, NULL }
    };

    void * globals;
    Function * main_fn;
    parse_tokens(&code, &globals, &main_fn);
    int ret = eval_main(main_fn, globals);
    printf("%d\n",ret);
}
