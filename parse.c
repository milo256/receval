#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#include "types.h"
#include "parse.h"

#define MAX_COLS 40

#define sof sizeof

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

Ident ident_new(u32 loc, u32 ofs) { return (loc << 30) | ofs; }
u32 var_location(Ident ident) { return ((3 << 30) & ident) >> 30; }
u32 var_offset(Ident ident) { return (((1 << 30) - 1) & ident); }


/* Note: receval does not have classes. the word "class" anywhere in this codebase just means "kind" or "category",
 * and does not have anything to do with the programming language construct. TypeClass refers to the overall type.
 * e.g. Function pointers are TypeClass `TYPE_FN_PTR` but Type `{ TYPE_FN_PTR, <return_type> }`
 */

typedef struct Type {
    TypeClass class;
    void * data;
} Type;

typedef struct {
    Type ret_type;
    u32 args_len;    
    u32 args_cap;
    /* and then args array is stored immediately after this in memory (trust) */
}  FunctionTypeData;

/* we don't need ident because ident is just offset + location,
 * and location is which array the def is stored in */
typedef struct {
    LStr name;
    Type type;
    u32 offset;
    void * init; /* location of the initialiser in code. only used for static
                   * variables since they're initialised prior to code execution. */
    LStr * arg_names; /* only used for functions */
} Def;

/* -- TOKENS -- */
typedef enum {
    TK_INT,
    TK_FLOAT,
    TK_STRING,
    TK_FUNCTION,
    TK_ARROW,
    TK_IDENT,
    TK_ASSIGN,
    TK_OPEN,
    TK_CLOSE, 
    TK_SB_OPEN,
    TK_SB_CLOSE,
    TK_CB_OPEN,
    TK_CB_CLOSE,
    TK_EOF,
    TK_NONE
} TkClass;

typedef struct {
    TkClass class;
    LStr val;
    u32 dbug_line;
    u32 dbug_column;
} Token;

Type * ret_type(Type * function) {
    ASSERT(function->class == TYPE_FN_PTR);
    return &((FunctionTypeData *) function->data)->ret_type;
}

Type * get_arg_types(Type * function) {
    ASSERT(function->class == TYPE_FN_PTR);
    return (Type *) ((void *) function->data + sizeof(FunctionTypeData));
}

u32 get_args_len(Type * function) {
    ASSERT(function->class == TYPE_FN_PTR);
    return ((FunctionTypeData *) function->data)->args_len;
}

Type type_function_ptr(Type ret_type) {
    FunctionTypeData data = {
        .ret_type = ret_type,
        .args_len = 0,
        .args_cap = 10
    };
    void * ptr = malloc(sizeof(FunctionTypeData) + data.args_cap * sizeof(Type));
    *(FunctionTypeData *)ptr = data;
    return (Type) {
        .class = TYPE_FN_PTR,
        .data = ptr
    };
}

bool type_eq(Type * a, Type * b) {
    if (a->class != b->class) return false;
    if (a == b) return true;
    switch (a->class) {
        case TYPE_INT: return true;
        case TYPE_FN_PTR: {
            u32 alen = get_args_len(a);
            u32 blen = get_args_len(b);
            if (alen != blen) return false;
            Type * atypes = get_arg_types(a);
            Type * btypes = get_arg_types(b);
            for (u32 i = 0; i < alen; i++) {
                if (!type_eq(&atypes[i], &btypes[i]))
                    return false;
            }
            return true;
        }
        default: PANIC();
    }
    return SATISFY_COMPILER;
}

void arg_type_push(Type * function, Type arg_type) {
    ASSERT(function->class == TYPE_FN_PTR);
    FunctionTypeData * data = (FunctionTypeData *) function->data;

    if (++(data->args_len) > data->args_cap) {
        data->args_cap *= 2;
        function->data = realloc(function->data, sizeof(FunctionTypeData) + data->args_cap * sizeof(Type));
    }
    Type * types = (void *) function->data + sizeof(FunctionTypeData);
    types[data->args_len - 1] = arg_type;
}

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

u32 long_token_class(LStr str) {
    if (isdigit(str.chars[0]))
        return TK_INT;
    if (lstr_eq(str, LSTR("function")))
        return TK_FUNCTION;
    return TK_IDENT;
}

u32 symbolic_token_class(LStr str) {
    if (str.len == 1) switch(str.chars[0]) {
        case '=': return TK_ASSIGN;
        case '(': return TK_OPEN;
        case ')': return TK_CLOSE;
        case '{': return TK_CB_OPEN;
        case '}': return TK_CB_CLOSE;
        default: PANIC();
    } else if (str.len == 2) {
        if (str.chars[0] == '-' && str.chars[1] == '>')
            return TK_ARROW;
    }
    PANIC();
    return SATISFY_COMPILER;
}

void print_tokens(Token * tokens) {
    while (tokens->class != TK_EOF) {
        char * buf = malloc(tokens->val.len + 1);
        buf[tokens->val.len] = 0;
        strncpy(buf, tokens->val.chars, tokens->val.len);
        printf("%d(%s)\n", tokens->class, buf);
        free(buf);
        tokens++;
    }
}

/* by far the worst function */
Token * tokenize(char * code) {
#define push_token(class) \
    do { \
        if (++tokens_len > tokens_cap) { \
            tokens_cap *= 2; tokens = realloc(tokens, tokens_cap * sizeof(Token)); \
        } \
        tokens[tokens_len-1] = (Token) { class, token_str, line, ts - line_start }; \
    } while(0)

    char char_tokens[] = "=(){}[]";

    u32 tokens_len = 0;
    u32 tokens_cap = 2;
    Token * tokens = malloc(tokens_cap * sizeof(Token));

    u32 line = 1;
    u32 line_start = 0;
    u32 ts = 0; /* token start */
    u32 tlen = 0; /* token len */
    char ch;

    LStr token_str;

    while ((ch = code[ts + tlen])) {
        bool char_token = strchr(char_tokens, ch);
        bool arrow = (ch == '-' && code[ts+tlen+1] == '>');
        bool comment_start = (ch == '/' && code[ts+tlen+1] == '*');

        if (!(comment_start || arrow || char_token || isspace(ch))) {
            tlen++;
            continue;
        }
        if (ch == '\n') {
            line++;
            line_start = ts + tlen;
        }
        if (tlen) {
            /* scanned a token */
            token_str = (LStr) { &code[ts], tlen };
            push_token(long_token_class(token_str));
            ts += tlen;
            tlen = 0;
        }
        if (char_token) {
            token_str = (LStr) { &code[ts], 1 };
            push_token(symbolic_token_class(token_str));
            ts++;
        }
        if (arrow) {
            token_str = (LStr) { &code[ts], 2 };
            push_token(TK_ARROW);
            ts++;
        }
        if (!(tlen || char_token)) {
            ts++;
        }

        if (comment_start) {
            do {
                if (ch == '\n') {
                    line++;
                    line_start = ts + tlen;
                }
                ts++;
            } while ((ch = code[ts]) && !(ch == '*' && code[ts+1] == '/'));
            ts += 2;
        }
    }

    token_str = (LStr) { &code[ts - 1], 1 };
    push_token(TK_EOF);

    return tokens;
}
#undef push_token


bool name_in(LStr name, Def * defs, u32 defs_len, u32 * out_index) {
    for (*out_index = 0; *out_index < defs_len; (*out_index)++)
        if (lstr_eq(defs[*out_index].name, name)) return true;
    return false;
}

void parse_expr(Token *, Def *, u32, Def *, u32, Def **, u32 *, u32 *, Expr *, Type *, Token **);

void parse_function_call_args(
    TkClass close,
    Token ** token_ptr, Def * global_defs, u32 globals_len, Def * outer_arg_defs,
    u32 outer_args_len, Def ** local_defs, u32 * locals_len, u32 * locals_cap,
    Expr ** out_args, u32 * out_args_len, u32 * out_argsb
) {
    Expr * args;
    u32 args_cap = 2, args_len = 0, argsb = 0;

    args = malloc(args_cap * sof(Expr));

    while((**token_ptr).class != close) {
        if (args_len >= args_cap) {
            args_cap *= 2;
            args = realloc(args, args_cap * sof(Expr));
        }
        Type arg_type;
        parse_expr(
            *token_ptr, global_defs, globals_len, outer_arg_defs, outer_args_len,
            local_defs, locals_len, locals_cap,
            &args[args_len], &arg_type, token_ptr
        );
        argsb += sof_type[arg_type.class];
        args_len++;
    }

    *out_args = args;
    *out_argsb = argsb;
    *out_args_len = args_len;
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
        if (lstr_eq(str, LSTR(builtin_names[i]))) return i;
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
                .ret_class = TYPE_INT,
                .expr = malloc(sof(Literal))
            };
            Literal literal = (Literal) { .val = malloc(sof(Integer)) };
            *(Integer *)(literal.val) = integer;
            *(Literal *)(out_expr->expr) = literal;
            *out_next_token = &tokens[1];
            return;
        } case TK_CB_OPEN: {
            OpBuiltin op = (OpBuiltin) { .class = B_SEQ };
            Token * token_ptr = &tokens[1];
            parse_function_call_args(
                TK_CB_CLOSE, &token_ptr,
                global_defs, globals_len, arg_defs, args_len,
                local_defs, locals_len, locals_cap,
                &op.args, &op.args_len, &op.argsb
            );
            *out_expr = (Expr) { OP_BUILTIN, TYPE_INT, malloc(sof(OpBuiltin)) };
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
                    &op.args, &op.args_len, &op.argsb
                );
                /* TODO: figure out return types for builtins */
                *out_expr = (Expr) { OP_BUILTIN, TYPE_INT, malloc(sof(OpBuiltin)) };
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

            Expr var_expr = (Expr) { OP_VAR, var_type.class, malloc(sof(OpVar)) };
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
                &call.args, &call.args_len, &call.argsb
            );
            
            //Type * expected_arg_types = get_arg_types(&var_type);
            u32 expected_args_len = get_args_len(&var_type);

            if (expected_args_len != call.args_len)
                error(&tokens[2], "incorrect number of arguments provided");

            *out_ret_type = *ret_type(&var_type);

            *out_expr = (Expr) { OP_CALL, out_ret_type->class, malloc(sof(OpCall)) };
            *(OpCall *)(out_expr->expr) = call;

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
            if (!type_eq(&var_type, &val_type))
                error(&tokens[2], "mismatched types");

            Expr expr = (Expr) { OP_ASSIGN, var_type.class, malloc(sof(OpAssign)) };
            *(OpAssign*)expr.expr = (OpAssign) {
                .ident = ident,
                .val = val,
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
    if (out_type) (*out_type = (Type) { .class = TYPE_FN_PTR, .data = NULL }); /* TODO: ARGS */
    return fn;
}

TypeClass lstr_to_type_class(LStr str) {
    if (lstr_eq(str, LSTR("int"))) return TYPE_INT;
    else return TYPE_NONE;
}

int delim(Token * token) {
    if (token->class == TK_OPEN || token->class == TK_CB_OPEN) return 1;
    else if (token->class == TK_CLOSE || token->class == TK_CB_CLOSE) return -1;
    else return 0;
}

/* TODO: doesn't work for assignments not within a function call */
void skip_to_end(Token ** token) {
    Token * starting_token = *token;
    if (delim(*token) < 1) {
        (*token)++;
        if (delim(*token) < 1) return;
    }
    (*token)++;
    for (u32 d = 1; d;) {
        if ((*token)->class == TK_EOF) error(starting_token, "missing closing delimiter");
        d += delim(*token);
        (*token)++;
    }
}

/* returns 0 if successful, 1 if error */
/* TODO: implement :P */
int parse_type(Token ** token, Type * out_type) {
    *out_type = (Type) { lstr_to_type_class((*token)->val), NULL };
    return 0;
}

void get_arg_defs(Def * function_def, Def ** out_arg_defs) {
    ASSERT(function_def->type.class == TYPE_FN_PTR);
    u32 args_len = get_args_len(&function_def->type);
    Type * types = get_arg_types(&function_def->type);
    LStr * names = function_def->arg_names;
    *out_arg_defs = malloc(sizeof(Def) * args_len);

    u32 arg_offset = 0;
    
    for (u32 i = 0; i < args_len; i++) {
        (*out_arg_defs)[i] = (Def) {
            .name = names[i], .type = types[i],
            .offset = arg_offset, .init = NULL,
        };
        arg_offset += sof_type[types[i].class];
    } 
}

void parse_global_def(Token ** tokens, Def ** global_defs, u32 * globals_len, u32 * globalsb, u32 * globals_cap) {
    Token * token = *tokens;
    if (token->class != TK_ASSIGN) error(token, "expected global variable definition");
    token++;
    if (token->class != TK_IDENT) error(token, "expected identifier");

    LStr var_name = token->val;
    Type type;

    LStr * arg_names = NULL;
    
    Token * init_token;
    token++;
    if (token->class == TK_FUNCTION) {
        token++;
        if (token->class != TK_ARROW) error(token, "expected return type `-> <type>`");
        
        token++;
        Type ret_type;
        if (parse_type(&token, &ret_type)) error(token, "expected type annotation");

        type = type_function_ptr(ret_type); 

        token++;

        if (token->class != TK_OPEN) error(token, "expected function arguments list `(...)`");

        token++;
        
        u32 arg_names_len = 0;
        u32 arg_names_cap = 2;
        arg_names = malloc(arg_names_cap * sizeof(LStr));

        while (token->class != TK_CLOSE) {
            ASSERT(token->class != TK_EOF);
            Type arg_type;
            if (parse_type(&token, &arg_type)) error(token, "expected type annotation");
            arg_type_push(&type, arg_type);
            token++;
            if (token->class != TK_IDENT) error(token, "expected argument name");
            ARRPUSH(token->val, arg_names, arg_names_len, arg_names_cap);
            token++;
        }
        token++;
        init_token = token;
        skip_to_end(&token);
    } else if (token->class == TK_INT) {
        type = (Type) { .class = TYPE_INT };
        init_token = token;
        token++;
    } else error(token, "value expected for global definition");
    
    Def def = (Def) {
        .name = var_name, .offset = *globalsb,
        .type = type, .init = init_token, .arg_names = arg_names
    };
    ARRPUSH(def, *global_defs, *globals_len, *globals_cap);
    *globalsb += sof_type[type.class];
    *tokens = token;
}

void parse_tokens(Token ** tokens, void ** out_globals, Function ** out_main) {
    u32 globals_len = 0, globalsb = 0, globals_cap = 2;
    Def * global_defs = malloc(globals_cap * sizeof(Def));
    *out_main = NULL;

    while((*tokens)->class != TK_EOF)
        parse_global_def(tokens, &global_defs, &globals_len, &globalsb, &globals_cap);
    
    void * globals = malloc(globalsb);

    for (u32 i = 0; i < globals_len; i++) {
        Def * def = &global_defs[i];
        Def * arg_defs;
        switch (def->type.class) {
            case TYPE_FN_PTR:
                get_arg_defs(def, &arg_defs);
                Function * fn = malloc(sof(Function));
                *fn = parse_function_code(
                    def->init, global_defs, globals_len,
                    arg_defs, get_args_len(&def->type), NULL
                ); 
                *(Function **)(globals + def->offset) = fn;
                if (lstr_eq(def->name, LSTR("main"))) *out_main = fn;
                break;
            case TYPE_INT:
                ASSERT(((Token *)def->init)->class == TK_INT);
                *(Integer *)(globals + def->offset) = lstr_to_int(((Token*)def->init)->val);
                break;
            default: PANIC();
        }
    }
    *out_globals = globals;
}

void parse_code(char * code, void ** out_globals, Function ** out_main) {
    Token * tokens = tokenize(code);
    parse_tokens(&tokens, out_globals, out_main);
}
