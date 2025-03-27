#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "da.h"
#include "arena.h"
#include "common.h"
#include "tokenizer.h"
#include "parse.h"

#define MAX_COLS 40

#define sof sizeof

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
    u32 param_count;    
    /* and then args array is stored immediately after this in memory (trust) */
}  FunctionTypeData;

/* we don't need ident because ident is just offset + location,
 * and location is which array the def is stored in */
typedef struct {
    LStr name, * param_names; /* param_names only used for functions */
    Type type;
    u32 offset;
    Token * init; /* location of the initialiser in code. only used for static
                   * variables since they're initialised prior to code execution. */
} Def;

typedef struct {
    struct {
        const Def * items;
        const u32 len;
    } globals;
    da(Def) locals;
} Context;

typedef da(Def) DefList;

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

/* freed after parsing */
static Arena parser_arena;

/* freed after execution */
static Arena code_arena;

static Type * get_ret_type(Type function) {
    ASSERT(function.class == TYPE_FN_PTR);
    return &((FunctionTypeData *) function.data)->ret_type;
}


static Type * get_param_types(Type function) {
    ASSERT(function.class == TYPE_FN_PTR);
    return (Type *) ((void *) function.data + sizeof(FunctionTypeData));
}


static u32 get_param_count(Type function) {
    ASSERT(function.class == TYPE_FN_PTR);
    return ((FunctionTypeData *) function.data)->param_count;
}


static Type make_type_fn_ptr(Type ret_type, u32 param_count) {
    FunctionTypeData data = {
        .ret_type = ret_type,
        .param_count = param_count,
    };
    void * ptr = aalloc(
        &parser_arena,
        sizeof(FunctionTypeData) + data.param_count * sizeof(Type)
    );
    *(FunctionTypeData *)ptr = data;
    return (Type) {
        .class = TYPE_FN_PTR,
        .data = ptr
    };
}

static Type make_type_int() { return (Type) { .class = TYPE_INT }; }

static Expr make_expr_literal(TypeClass type_class, void ** val_ptr) {
    Expr expr = {
        .class = LITERAL,
        .ret_class = type_class,
        .expr = aalloc(&code_arena, sof(Literal))
    };
    Literal literal = { .val = aalloc(&code_arena, sof_type[type_class]) };
    *val_ptr = literal.val;
    *(Literal *)(expr.expr) = literal;
    return expr;
}

static Expr make_expr_builtin(BuiltinClass class, TypeClass ret_class, OpBuiltin ** op_ptr) {
    Expr expr = { OP_BUILTIN, ret_class, aalloc(&code_arena, sof(OpBuiltin)) };
    *op_ptr = expr.expr;
    **op_ptr = (OpBuiltin) { .class = class };
    return expr;
}

static Expr make_expr_call(Expr fn, TypeClass ret_class, OpCall ** op_ptr) {
    Expr expr = { OP_CALL, ret_class, aalloc(&code_arena, sof(OpCall)) };
    *op_ptr = expr.expr;
    **op_ptr = (OpCall) { .fn = fn };
    return expr;
}

static Expr make_expr_var(TypeClass type_class, Ident ident) {
    Expr expr = { OP_VAR, type_class, aalloc(&code_arena, sof(OpVar)) };
    OpVar * op = expr.expr;
    op->ident = ident;
    return expr;
}


static bool type_eq(const Type * a, const Type * b) {
    if (a->class != b->class) return false;
    if (a == b) return true;
    switch (a->class) {
        case TYPE_INT: return true;
        case TYPE_FN_PTR: {
            u32 alen = get_param_count(*a);
            u32 blen = get_param_count(*b);
            if (alen != blen) return false;
            Type * atypes = get_param_types(*a);
            Type * btypes = get_param_types(*b);
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


static void error(Token const * tk, char * msg) {
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

static void parse_expr(
    Token const ** tokens,
    Context * context,
    Expr * out_expr, Type * out_ret_type
);


static void parse_call_params(
    TkClass close,
    Token const ** tokens,
    Context * context,
    Expr ** out_args, u32 * out_args_len
) {
    da(Expr) args = {};

    while((*tokens)->class != close) {
        Expr arg;
        Type arg_type;
        parse_expr(
            tokens, context,
            &arg, &arg_type
        );
        da_append(args, arg);
    }

    u32 n = args.len * sizeof(Expr);
    *out_args = aalloc(&code_arena, n);
    memcpy(*out_args, args.items, n);
    *out_args_len = args.len;

    da_dealloc(args);
}

static Ident add_local(Context * ctx, LStr name, Type type) {
    u32 offset = ctx->locals.len ?
        ctx->locals.items[ctx->locals.len - 1].offset
        + sof_type[ctx->locals.items[ctx->locals.len - 1].type.class] : 0;

    Ident ident = ident_new(LOCAL, offset);

    Def def = {
        .name = name,
        .offset = var_offset(ident),
        .type = type
    };

    da_append(ctx->locals, def);
    return ident;
}

static TypeClass lstr_to_type_class(LStr str) {
    if (lstr_eq(str, LSTR("int"))) return TYPE_INT;
    else return TYPE_NONE;
}

static int delim(const Token * token) {
    if (token->class == TK_OPEN || token->class == TK_CB_OPEN) return 1;
    else if (token->class == TK_CLOSE || token->class == TK_CB_CLOSE) return -1;
    else return 0;
}

/* TODO: doesn't work for assignments not within a function call */
static void skip_to_end(const Token ** token) {
    const Token * starting_token = *token;
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

/* TODO: implement :P */
static Type parse_type(const Token ** token) {
    Type ret = { lstr_to_type_class((*token)->val), NULL };
    (*token)++;
    return ret;
}

static void create_param_defs(const Def * function_def, Def * buf) {
    ASSERT(function_def->type.class == TYPE_FN_PTR);
    u32 offset = 0,
        param_count = get_param_count(function_def->type);

    Type * types = get_param_types(function_def->type);
    LStr * names = function_def->param_names;
    
    for (u32 i = 0; i < param_count; i++) {
        buf[i] = (Def) {
            .name = names[i], .type = types[i],
            .offset = offset, .init = NULL,
        };
        offset += sof_type[types[i].class];
    } 
}

#define defs_size(list) \
        ((list).len ? (list).items[(list).len - 1].offset + sof_type[(list).items[(list).len - 1].type.class] : 0)


static bool name_in(LStr name, const Def * defs, u32 defs_len, u32 * out_index) {
    for (*out_index = 0; *out_index < defs_len; (*out_index)++)
        if (lstr_eq(defs[*out_index].name, name)) return true;
    return false;
}

static bool parse_var(
    LStr var_name,
    Context context,
    Type * out_type, Ident * out_ident
) {
    u32 var_index;
    if (name_in(var_name, context.locals.items, context.locals.len, &var_index)) {
        *out_type = context.locals.items[var_index].type;
        *out_ident = ident_new(LOCAL, context.locals.items[var_index].offset);
    } else if (name_in(var_name, context.globals.items, context.globals.len, &var_index)) {
        *out_type = context.globals.items[var_index].type;
        *out_ident = ident_new(GLOBAL, context.globals.items[var_index].offset);
    } else return false;
    return true;
}

static BuiltinClass lstr_to_builtin(LStr str) {
    for (BuiltinClass i = 0; i < ARRLEN(builtin_names); i++) {
        if (lstr_eq(str, LSTR(builtin_names[i]))) return i;
    }
    return B_NONE;
}

static Integer lstr_to_int(LStr str) {
    char * end_ptr;
    Integer ret = strtol(str.chars, &end_ptr, 10); 
    ASSERT(end_ptr == str.chars + str.len);
    return ret;
}

static void parse_expr(
    Token const ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
) {
    switch ((*tokens)->class) {
        case TK_INT: {
            Integer * value_ptr;
            *out_ret_type = make_type_int();
            *out_expr = make_expr_literal(TYPE_INT, (void **) &value_ptr);
            *value_ptr = lstr_to_int((*tokens)->val);
            (*tokens)++;
            return;
        } case TK_CB_OPEN: {
            OpBuiltin * op;
            *out_expr = make_expr_builtin(B_SEQ, TYPE_INT, &op);
            (*tokens) += 1;
            parse_call_params(
                TK_CB_CLOSE, tokens, context, &op->args, &op->args_len
            );
            *out_ret_type = make_type_int();
            (*tokens)++;
            return;
        } case TK_IDENT: {
            /* ident as expr if not a builtin is a variable, maybe a function call */
            Type var_type;
            Ident ident;

            BuiltinClass bclass = lstr_to_builtin((*tokens)->val);

            if (bclass != B_NONE) {
                OpBuiltin * op;
                *out_expr = make_expr_builtin(bclass, TYPE_INT, &op);
                (*tokens) += 2;
                parse_call_params(
                    TK_CLOSE, tokens, context, &op->args, &op->args_len
                );
                *out_ret_type = make_type_int();
                (*tokens)++;
                return;
            }

            if (!parse_var(
                (*tokens)->val, *context,
                &var_type, &ident
            )) error(*tokens, "unknown identifier");

            Expr var_expr = make_expr_var(var_type.class, ident);

            if ((*tokens)[1].class != TK_OPEN) {
                *out_expr = var_expr;
                *out_ret_type = var_type;
                (*tokens)++;
                return;
            }

            if (var_type.class != TYPE_FN_PTR)
                error(*tokens + 1, "not a function");

            OpCall * call;
            *out_expr = make_expr_call(var_expr, get_ret_type(var_type)->class, &call);

            
            Token const * token_ptr = *tokens + 2;
            parse_call_params(
                TK_CLOSE, &token_ptr, context,
                &call->args, &call->args_len
            );
            
            //Type * expected_arg_types = get_arg_types(&var_type);
            u32 expected_args_len = get_param_count(var_type);

            if (expected_args_len != call->args_len)
                error(*tokens + 2, "incorrect number of arguments provided");

            *out_ret_type = *get_ret_type(var_type);

            *tokens = token_ptr + 1;

            return;
        } case TK_ASSIGN: {
            (*tokens)++;
            if ((*tokens)->class != TK_IDENT)
                error(*tokens + 1, "expected identifier");

            LStr var_name = (*tokens)->val;
            (*tokens)++;

            Expr val;
            Type val_type;
            parse_expr(
                tokens, context,
                &val, &val_type
            );
            

            Type var_type;
            Ident var_ident;

            if (parse_var(
                var_name, *context, &var_type, &var_ident
            )) {
                if (!type_eq(&var_type, &val_type))
                    error(*tokens + 2, "mismatched types");
            } else {
                var_type = val_type;
                var_ident = add_local(context, var_name, var_type);
            }


            Expr expr = (Expr) { OP_ASSIGN, var_type.class, malloc(sof(OpAssign)) };
            *(OpAssign*)expr.expr = (OpAssign) {
                .ident = var_ident,
                .val = val,
            };
            *out_expr = expr;
            *out_ret_type = var_type;
            return; 
        }
        default: error(*tokens, "expected expression");
    }
}

static Function parse_function_code(
    const Def * def, const Def * global_defs, u32 globals_len,
    Type * out_type
) {
    Function fn;
    Type ret_type;

    u32 param_count = get_param_count(def->type);

    Context ctx = {
        { global_defs, globals_len },
        da_new(Def, param_count),
    };

    create_param_defs(def, ctx.locals.items);
    ctx.locals.len = param_count;

    const Token * end_token = def->init;
    parse_expr(
        &end_token, &ctx,
        &fn.body, &ret_type
    );

    fn.stack_size = defs_size(ctx.locals);
    if (out_type)
        (*out_type = (Type) { .class = TYPE_FN_PTR, .data = NULL }); /* TODO: ARGS */

    da_dealloc(ctx.locals);
    return fn;
}


static u32 parser_count_params(const Token * open) {
    const Token * token = open;
    if (token->class != TK_OPEN) error(token, "expected function arguments list `(...)`");
    token++; 
    u32 ct = 0;
    while (token->class != TK_EOF && token->class != TK_CLOSE) {
        if (parse_type(&token).class == TYPE_NONE) error(token, "expected type");
        if (token->class != TK_IDENT) error(token, "expected identifier");
        token++;
        ct++;
    }
    if (token->class != TK_CLOSE) error(open, "unclosed delimiter");
    return ct;
}

static void parse_global_def(const Token ** tokens, DefList * globals) {
    const Token * token = *tokens;
    if (token->class != TK_ASSIGN) error(token, "expected global variable definition");
    token++;
    if (token->class != TK_IDENT) error(token, "expected identifier");

    LStr var_name = token->val;
    Type type;

    LStr * param_names = NULL;
    
    const Token * init_token;
    token++;
    if (token->class == TK_FUNCTION) {
        token++;
        if (token->class != TK_ARROW) error(token, "expected return type `-> <type>`");
        
        token++;
        Type ret_type = parse_type(&token);
        if (ret_type.class == TYPE_NONE) error(token, "expected type");

        u32 param_count = parser_count_params(token);
        param_names = aalloc(&code_arena, sizeof(LStr) * param_count);
        token++;

        type = make_type_fn_ptr(ret_type, param_count); 
        for (u32 i = 0; i < param_count; i++) {
            get_param_types(type)[i] = parse_type(&token);
            param_names[i] = token++->val;
        }
        ASSERT(token->class == TK_CLOSE);
        token++;
        init_token = token;
        skip_to_end(&token);
    } else if (token->class == TK_INT) {
        type = (Type) { .class = TYPE_INT };
        init_token = token;
        token++;
    } else error(token, "value expected for global definition");
    
    Def def = (Def) {
        .name = var_name, .offset = defs_size(*globals),
        .type = type, .init = (Token *) init_token, .param_names = param_names
    };
    da_append(*globals, def);
    *tokens = token;
}

static void parse_tokens(const Token ** tokens, void ** out_globals, Function ** out_main) {
    DefList global_defs = {};
    *out_main = NULL;

    while((*tokens)->class != TK_EOF)
        parse_global_def(tokens, &global_defs);

    void * global_values = aalloc(&code_arena, defs_size(global_defs));

    for (u32 i = 0; i < global_defs.len; i++) {
        Def * def = &global_defs.items[i];
        switch (def->type.class) {
            case TYPE_FN_PTR: {
                Function * fn = aalloc(&code_arena, sizeof(Function));
                *fn = parse_function_code(
                    def, global_defs.items, global_defs.len, NULL
                ); 
                *(Function **)(global_values + def->offset) = fn;
                if (lstr_eq(def->name, LSTR("main"))) *out_main = fn;
                break;
            } case TYPE_INT:
                ASSERT(((Token *)def->init)->class == TK_INT);
                *(Integer *)(global_values + def->offset) = lstr_to_int(((Token*)def->init)->val);
                break;
            default: PANIC();
        }
    }
    *out_globals = global_values;

    da_dealloc(global_defs);
}

void free_code(void) {
    afree(code_arena);
}

void parse_code(char * code, void ** out_globals, Function ** out_main) {
    parser_arena = arena_init();
    code_arena = arena_init();
    const Token * tokens = tokenize(code, &code_arena);
    parse_tokens(&tokens, out_globals, out_main);
    afree(parser_arena);
}
