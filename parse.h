#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#include "builtin.h"
#include "types.h"
#include "macros.h"

#define MAX_COLS 40

#define sof sizeof

void error(Token * tk, char * msg) {
    fprintf(stderr, "Receval: Error on %d:%d: %s\n", tk->dbug_line, tk->dbug_column, msg);
    char * at = tk->val.chars;
    char * line_start = at - tk->dbug_column + 1;
    char * line_end = strchr(at, '\n');

    char * str_start = MAX(line_start, line_end - MAX_COLS);
    if ((at - 4) < str_start)
        str_start = MAX(at - 4, line_start);
    char * str_end = MIN(line_start + MAX_COLS, line_end);

    char buf[MAX_COLS + 1];
    u32 len = str_end - str_start;
    u32 error_pos = at - str_start;
    u32 error_len = MAX(1, MIN(tk->val.len, str_end - at));
    strncpy(buf, str_start, len);

    buf[len] = '\0';

    u32 margin_width = fprintf(stderr, " %d |", tk->dbug_line);
    fprintf(stderr, "%s\n", buf);
    for (u32 i = 0; i < error_pos + margin_width; i++)
        putchar(' ');
    for (u32 i = 0; i < error_len; i++)
        putchar('^');
    putchar('\n');
    exit(1);
}

int lstr_eq(LStr a, LStr b) {
    return a.len == b.len && !strncmp(a.chars, b.chars, MIN(a.len, b.len));
}

u32 get_token_class(LStr str) {
    if (str.len == 0)
        return TK_NONE;
    if (str.len == 1) switch(str.chars[0]) {
        case '=': return TK_ASSIGN;
        case '(': return TK_OPEN;
        case ')': return TK_CLOSE;
        case '{': return TK_BEGIN;
        case '}': return TK_END;
    }
    if (isdigit(str.chars[0]))
        return TK_INT;
    if (lstr_eq(str, LSTR("function")))
        return TK_FUNCTION;
    return TK_IDENT;
}

Token * tokenize(char * code) {
#define push_token(chars, len) \
    do { \
        LStr m_token_str = { chars, len }; \
        if (++tokens_len > tokens_cap) { \
            tokens_cap *= 2; tokens = realloc(tokens, tokens_cap * sizeof(Token)); \
        } \
        tokens[tokens_len-1] = (Token) { get_token_class(m_token_str), m_token_str, line, column - tlen }; \
    } while(0)
#define new_char() \
    if (code[ts + tlen] == '\n') { \
        column = 0; \
        line++; \
    } else { \
        column++; \
    }

    char char_tokens[] = "=(){}";
    u32 tokens_len = 0;
    u32 tokens_cap = 2;
    Token * tokens = malloc(tokens_cap * sizeof(Token));

    u32 line = 1;
    u32 column = 0;
    u32 ts = 0;
    u32 tlen = 0;
    char ch;

    while ((ch = code[ts + tlen])) {
        bool char_token = strchr(char_tokens, ch);
        if (!(char_token || isspace(ch))) {
            new_char();
            tlen++;
            continue; /* continue scanning */
        }

        if (tlen) {
            /* scanned a token */
            push_token(&code[ts], tlen);
            ts += tlen;
            tlen = 0;
        }
        if (char_token) {
            push_token(&code[ts], 1);
            new_char();
            ts++;
        }
        if (!(tlen || char_token)) {
            new_char();
            ts++;
        }
    }

    push_token("", 0);
    tokens[tokens_len-1].class = TK_EOF;

    return tokens;
}
#undef push_token
#undef new_char


bool name_in(LStr name, Def * defs, u32 defs_len, u32 * out_index) {
    for (*out_index = 0; *out_index < defs_len; (*out_index)++)
        if (lstr_eq(defs[*out_index].name, name)) return true;
    return false;
}

Ident ident_new(u32 loc, u32 ofs) { return (loc << 30) | ofs; }
u32 var_location(Ident ident) { return ((3 << 30) & ident) >> 30; }
u32 var_offset(Ident ident) { return (((1 << 30) - 1) & ident); }

void parse_expr(Token *, Def *, u32, Def *, u32, Def **, u32 *, u32 *, Expr *, Type *, Token **);

void parse_function_call_args(
    TkClass close,
    Token ** token_ptr, Def * global_defs, u32 globals_len, Def * outer_arg_defs,
    u32 outer_args_len, Def ** local_defs, u32 * locals_len, u32 * locals_cap,
    Expr ** out_args, u32 ** out_arg_typesb, u32 * out_args_len, u32 * out_argsb
) {
    Expr * args;
    u32 args_cap = 2, args_len = 0, argsb = 0, * arg_typesb;

    args = malloc(args_cap * sof(Expr));
    arg_typesb = malloc(args_cap * sof(u32));

    while((**token_ptr).class != close) {
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

bool parse_var(
    LStr var_name,
    Def * global_defs, u32 globals_len, Def * arg_defs,
    u32 args_len, Def * local_defs, u32 locals_len,
    Type * out_type, Ident * out_ident
) {
    u32 var_index;
    if (name_in(var_name, local_defs, locals_len, &var_index)) {
        *out_type = local_defs[var_index].type;
        *out_ident = ident_new(LOCAL, local_defs[var_index].offset);
    } else if (name_in(var_name, arg_defs, args_len, &var_index)) {
        *out_type = arg_defs[var_index].type;
        *out_ident = ident_new(ARGUMENT, arg_defs[var_index].offset);
    } else if (name_in(var_name, global_defs, globals_len, &var_index)) {
        *out_type = global_defs[var_index].type;
        *out_ident = ident_new(GLOBAL, global_defs[var_index].offset);
    } else return false;
    return true;
}

BuiltinClass lstr_to_builtin(LStr str) {
    for (BuiltinClass i = 0; i < ARRLEN(builtin_names); i++) {
        if (lstr_eq(str, LSTR(builtin_names[i])))
            return i;
    }
    return B_NONE;
}

Integer lstr_to_int(LStr str) {
    char * end_ptr;
    Integer ret = strtol(str.chars, &end_ptr, 10); 
    ASSERT(end_ptr == str.chars + str.len);
    return ret;
}

void parse_expr(
    Token * tokens,
    Def * global_defs, u32 globals_len, Def * arg_defs, u32 args_len,
    Def ** local_defs, u32 * locals_len, u32 * locals_cap,
    Expr * out_expr, Type * out_ret_type, Token ** out_next_token
) {
    switch (tokens->class) {
        case TK_INT: {
            Integer integer = lstr_to_int(tokens->val);
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
        } case TK_BEGIN: {
            OpBuiltin op = (OpBuiltin) { .class = B_SEQ };
            Token * token_ptr = &tokens[1];
            parse_function_call_args(
                TK_END, &token_ptr,
                global_defs, globals_len, arg_defs, args_len,
                local_defs, locals_len, locals_cap,
                &op.args, &op.arg_typesb, &op.args_len, &op.argsb
            );
            *out_expr = (Expr) { OP_BUILTIN, malloc(sof(OpBuiltin)) };
            *out_ret_type = (Type) { .class = TYPE_INT };
            *(OpBuiltin *)(out_expr->expr) = op;
            *out_next_token = &token_ptr[1];
            return;
        } case TK_IDENT: {
            /* ident as expr if not a builtin is a variable, maybe a function call */
            Type var_type;
            Ident ident;

            BuiltinClass bclass = lstr_to_builtin(tokens->val);

            if (bclass != B_NONE) {
                OpBuiltin op = (OpBuiltin) { .class = bclass };
                Token * token_ptr = &tokens[2];
                parse_function_call_args(
                    TK_CLOSE, &token_ptr,
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
                tokens[0].val,
                global_defs, globals_len, arg_defs, args_len, *local_defs, *locals_len,
                &var_type, &ident
            )) error(tokens, "unknown identifier");

            Expr var_expr = (Expr) { .class = OP_VAR, .expr = malloc(sof(OpVar)) };
            *(OpVar *)(var_expr.expr) = (OpVar) { .ident = ident };

            if (tokens[1].class != TK_OPEN) {
                *out_expr = var_expr;
                *out_ret_type = var_type;
                *out_next_token = &tokens[1];
                return;
            }

            if (var_type.class != TYPE_FN_PTR)
                error(&tokens[1], "not a function");

            OpCall call = (OpCall) { .fn = var_expr };
            
            Token * token_ptr = &tokens[2];
            parse_function_call_args(
                TK_CLOSE, &token_ptr, 
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
            if (tokens[1].class != TK_IDENT)
                error(&tokens[1], "expected identifier");

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
                tokens[1].val,
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
                    .name = tokens[1].val,
                    .offset = var_offset(ident),
                    .type = var_type
                };
                (*locals_len)++;
            }
            /* should be deep comparison */
            if (var_type.class != val_type.class)
                error(&tokens[2], "mismatched types");

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
        default: error(tokens, "expected expression");
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

    fn.localsb = locals_len ? local_defs[locals_len - 1].offset + sof_type[local_defs[locals_len - 1].type.class] : 0;
    if (out_type) (*out_type = (Type) { .class = TYPE_FN_PTR, .ret_type = ret_type });
    return fn;
}

Type lstr_to_type(LStr str) {
    if (lstr_eq(str, LSTR("int")))
        return (Type) { .class = TYPE_INT };
    else return (Type) { .class = TYPE_NONE };
}

int delim(Token * token) {
    if (token->class == TK_OPEN || token->class == TK_BEGIN)
        return 1;
    else if (token->class == TK_CLOSE || token->class == TK_END)
        return -1;
    else return 0;
}

/* TODO: doesn't work for assignments not within a function call */
void skip_to_end(Token ** token) {
    Token * starting_token = *token;
    if (delim(*token) < 1)
        (*token)++;
    if (delim(*token) < 1)
        return;
    (*token)++;
    for (u32 d = 1; d;) {
        if ((*token)->class == TK_EOF) error(starting_token, "missing closing delimiter");
        d += delim(*token);
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

    /* earlier stages in the parser should have already picked up on
     * missing or unpaired parenthesis */
    ASSERT(token->class == TK_OPEN);
    token++;
    while (token->class != TK_CLOSE) {

        Type type = lstr_to_type(token->val);
        if (token->class != TK_IDENT || type.class == TYPE_NONE)
            error(token, "expected type annotation");

        if (token[1].class != TK_IDENT)
            error(&token[1], "expected identifier");

        LStr name = token[1].val;

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
    
    if (out_function_code) *out_function_code = token;

    skip_to_end(&token);

    if (out_next_token) *out_next_token = token;
}

void parse_global_def(Token ** tokens, Def ** global_defs, u32 * globals_len, u32 * globalsb, u32 * globals_cap) {
    Token * token = *tokens;
    if (token->class != TK_ASSIGN)
        error(token, "expected global variable definition");
    token++;
    if (token->class != TK_IDENT)
        error(token, "expected identifier");

    LStr var_name = token->val;

    Type type;
    
    Token * init_token;
    token++;
    if (token->class == TK_FUNCTION) {
        type = (Type) {
            .class = TYPE_FN_PTR,
            .ret_type = malloc(sof(Type))
        };
        token++;
        Type ret_type = lstr_to_type(token->val);
        if (token->class != TK_IDENT || ret_type.class == TYPE_NONE)
            error(token, "expected type annotation");
        *(type.ret_type) = ret_type;

        token++;
        init_token = token;
        if (token->class != TK_OPEN)
            error(token, "expected function arguments list `(...)`");
        skip_to_end(&token);
        skip_to_end(&token);
    } else if (token->class == TK_INT) {
        type = (Type) { TYPE_INT };
        init_token = token;
        token++;
    } else error(token, "value expected for global definition");
    
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
                if (lstr_eq(def.name, LSTR("main"))) *out_main = fn;
                break;
            case TYPE_INT:
                ASSERT(((Token *)def.init)->class == TK_INT);
                *(Integer *)(globals + def.offset) = lstr_to_int(((Token*)def.init)->val);
                break;
            default: PANIC();
        }
    }
    *out_globals = globals;
}


