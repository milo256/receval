#include "parser.h"

#include "common.h"
#include "ident.h"
#include "arena.h"
#include "da.h"
#include "tokenizer.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>


/* Note: receval does not have classes. the word "class" anywhere in this
 * codebase just means "kind" or "category", and does not have anything to do
 * with the common programming language feature called a class.
 */

#define MAX_COLS 40
#define sof sizeof


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
                   * variables since they're initialised prior to execution. */
} Def;


typedef struct {
    struct {
        const Def * items;
        const u32 len;
    } globals;
    da(Def) locals;
} Context;


typedef da(Def) DefList;


/* freed after parsing */
static Arena parser_arena;

/* freed after execution */
static Arena code_arena;


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



/* Token Functions
 * -----------------------------------------------------------------------------
 */

static int delim_value(const Token * token) {
    int c = token->class;
    if (c > 0 && c <= TK_CB_CLOSE) return (c % 2)? 1 : -1;
    else return 0;
}


static int opposite_delim(const Token * token) {
    int c = token->class;
    return (c % 2)? c + 1 : c - 1;
}


/* TODO: doesn't work for assignments not within a function call */
static void skip_to_end(const Token ** token) {
    const Token * starting_token = *token;
    if (delim_value(*token) < 1) {
        (*token)++;
        if (delim_value(*token) < 1) return;
    }
    (*token)++;
    for (u32 d = 1; d;) {
        if ((*token)->class == TK_EOF) error(starting_token, "missing closing delimiter");
        d += delim_value(*token);
        (*token)++;
    }
}


static TypeClass lstr_to_type_class(LStr str) {
    if (lstr_eq(str, LSTR("int"))) return TYPE_INT;
    else if (lstr_eq(str, LSTR("str"))) return TYPE_STR;
    else return TYPE_NONE;
}



/* Type Functions
 * -----------------------------------------------------------------------------
 */

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


static bool type_eq(const Type * a, const Type * b) {
    if (a->class != b->class) return false;
    if (a == b) return true;
    switch (a->class) {
        case TYPE_INT: case TYPE_STR:
            return true;
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



/* Expression Functions
 * -----------------------------------------------------------------------------
 */

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



/* Definition Functions
 * -----------------------------------------------------------------------------
 */

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



/* String Parsing Functions
 * -----------------------------------------------------------------------------
 */

static BuiltinClass lstr_to_builtin(const LStr str) {
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

    for (BuiltinClass i = 0; i < ARRLEN(builtin_names); i++)
        if (lstr_eq(str, LSTR(builtin_names[i]))) return i;

    return B_NONE;
}


static Integer lstr_to_int(LStr str) {
    char * end_ptr;
    Integer ret = strtol(str.chars, &end_ptr, 10); 
    ASSERT(end_ptr == str.chars + str.len);
    return ret;
}



/* Token Parsing Functions
 * -----------------------------------------------------------------------------
 */

/* TODO: implement :P */
static Type parse_type(const Token ** token) {
    Type ret = { lstr_to_type_class((*token)->val), NULL };
    (*token)++;
    return ret;
}


static void parse_expr(
    Token const ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
);


static void parse_call_params(
    Token const ** tokens, Context * context,
    Type * expected_param_types, u32 expected_param_count,
    Expr ** out_params, u32 * out_param_count
) {
    Expr * params = aalloc(&code_arena, expected_param_count * sizeof(Expr));

    ASSERT(delim_value(*tokens) > 0);
    TkClass close = opposite_delim(*tokens);
    (*tokens)++;

    u32 i = 0;
    for(;
        (*tokens)->class != close
        && (*tokens)->class != TK_EOF
        && (i < expected_param_count);
        i++
    ) {
        Type param_type;
        const Token * param_token = *tokens;
        parse_expr(
            tokens, context,
            &params[i], &param_type
        );
        if (!type_eq(&param_type, &expected_param_types[i]))
            error(param_token, "mismatched parameter types");
    }

    if (i != expected_param_count)
        error(*tokens + 2, "incorrect number of parameters provided");

    *out_params = params;
    *out_param_count = i;
}


static void parse_variadic_call_params(
    Token const ** tokens, Context * context,
    Expr ** out_params, Type ** out_param_types, u32 * out_param_count
) {
    da(Expr) params = {};
    da(Type) param_types = {};

    ASSERT(delim_value(*tokens) > 0);
    TkClass close = opposite_delim(*tokens);
    (*tokens)++;

    u32 i = 0;
    for(; (*tokens)->class != close&& (*tokens)->class != TK_EOF; i++) {
        Expr param;
        Type param_type;
        parse_expr(
            tokens, context,
            &param, &param_type
        );
        da_append(params, param);
        da_append(param_types, param_type);
    }

    *out_params = aalloc(&code_arena, i * sizeof(Expr));
    *out_param_types = aalloc(&parser_arena, i * sizeof(Type));
    *out_param_count = i;

    memcpy(*out_params, params.items, i * sizeof(Expr));
    memcpy(*out_param_types, param_types.items, i * sizeof(Type));

    da_dealloc(params);
    da_dealloc(param_types);
}


static bool find_name(LStr name, const Def * defs, u32 defs_len, u32 * out_index) {
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
    if (find_name(var_name, context.locals.items, context.locals.len, &var_index)) {
        *out_type = context.locals.items[var_index].type;
        *out_ident = ident_new(LOCAL, context.locals.items[var_index].offset);
    } else if (find_name(var_name, context.globals.items, context.globals.len, &var_index)) {
        *out_type = context.globals.items[var_index].type;
        *out_ident = ident_new(GLOBAL, context.globals.items[var_index].offset);
    } else return false;
    return true;
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
            Type * arg_types;
            parse_variadic_call_params(
                tokens, context, &op->args, &arg_types, &op->args_len
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
                (*tokens)++;
                if ((*tokens)->class != TK_OPEN) error(*tokens, "expected `(`");
                Type * arg_types;
                parse_variadic_call_params(
                    tokens, context, &op->args, &arg_types, &op->args_len
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

            
            Token const * token_ptr = *tokens + 1;
            parse_call_params(
                &token_ptr, context, get_param_types(var_type), get_param_count(var_type),
                &call->args, &call->args_len
            ); 

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

    u32 param_count = get_param_count(def->type);

    Context ctx = {
        { global_defs, globals_len },
        da_new(Def, param_count),
    };

    create_param_defs(def, ctx.locals.items);
    ctx.locals.len = param_count;

    const Token * token = def->init;
    Type ret_type;
    parse_expr(
        &token, &ctx,
        &fn.body, &ret_type
    );

    Type * expected_ret_type = get_ret_type(def->type);
    if (!type_eq(&ret_type, expected_ret_type)) error(def->init, "mismatched return types");

    fn.stack_size = defs_size(ctx.locals);
    if (out_type) *out_type = def->type;

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
        const Token * ret_type_token = token;
        Type ret_type = parse_type(&token);
        if (ret_type.class == TYPE_NONE) error(ret_type_token, "expected type");

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



/* API Functions
 * -----------------------------------------------------------------------------
 */

void free_code(void) { afree(code_arena); }


void parse_code(char * code, void ** out_globals, Function ** out_main) {
    parser_arena = arena_init();
    code_arena = arena_init();
    const Token * tokens = tokenize(code, &code_arena);
    parse_tokens(&tokens, out_globals, out_main);
    afree(parser_arena);
}
