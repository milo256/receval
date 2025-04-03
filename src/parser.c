#include "parser.h"

#include "common.h"
#include "expr.h"
#include "tokenizer.h"
#include "types.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>


/* Note: receval does not have classes. the word "class" anywhere in this
 * codebase just means "kind" or "category", and does not have anything to do
 * with the common programming language feature called a class.
 */

#define MAX_COLS 40



/* Definition Types
 * -----------------------------------------------------------------------------
 */

typedef struct {
    slice_t name;
    Type type;
    Ident ident;
} Def;


typedef da(Def) DefList;

/* in the global context, locals == globals. is this confusing? maybe.
 * TODO: think of a better name */
typedef struct {
    DefList * globals;
    DefList * locals;
    u32 next_index, next_offset;
} Context;



/* Memory Arenas
 * -----------------------------------------------------------------------------
 */

/* freed after parsing */
static Arena * parser_arena;

/* freed after execution */
static Arena * code_arena;



/* Error Handling
 * -----------------------------------------------------------------------------
 */

static void error_internal(Token const * tk, char * msg) {
    fprintf(
        stderr, "Receval: Error on %d:%d: %s\n",
        tk->dbug_line, tk->dbug_column, msg
    );
    char * at = tk->val.sptr;
    char * line_start = at - tk->dbug_column + 1;
    char * line_end = strchr(at, '\n');

    char * str_start = max(line_start, line_end - MAX_COLS);
    if ((at - 4) < str_start)
        str_start = max(at - 4, line_start);
    char * str_end = min(line_start + MAX_COLS, line_end);

    char buf[MAX_COLS + 1];
    u32 len = str_end - str_start;
    u32 error_pos = at - str_start;
    u32 error_len = max(1, min(slicelen(tk->val), (size_t) (str_end - at)));
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

#ifdef NDEBUG
#define error(tk, msg) error_internal(tk, msg)
#else
#define error(tk, msg) do {\
    fprintf(stderr, "DEBUG: error emitted on %s:%d\n", __FILE__, __LINE__); \
    error_internal(tk, msg); \
} while (0)

#endif



/* Token Functions
 * -----------------------------------------------------------------------------
 */

static int delim_value(const Token * token) {
    int c = token->class;
    if (c > 0 && c <= TK_CB_CLOSE) return (c % 2)? 1 : -1;
    else return 0;
}


static u32 opposite_delim(u32 class) {
    return (class % 2)? class + 1 : class - 1;
}


/* TODO: fix this, it's missing a bunch of cases */
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


static TypeClass str_to_type_class(slice_t str) {
    for (u32 i = 0; i < arrlen(type_names); i++) {
        if (!type_names[i]) continue;
        if (slice_str_eq(str, type_names[i])) return i;
    }
    return TYPE_NONE;
}



/* Type Functions
 * -----------------------------------------------------------------------------
 */

static bool is_type_primitive(TypeClass class) {
    return (class < FIRST_COMPLEX_TYPE);
}


static u32 sizeof_type(int type_class) {
    assert(type_class != TYPE_NONE);
    assert(type_class != TYPE_UNKNOWN);
    u32 size = type_sizes[type_class];
    assert(size <= TYPE_MAX_SIZE);
    return size;
}


static Type * get_ret_type(Type function) {
    assert_eq(function.class, TYPE_FUNCTION);
    return &((FunctionTypeData *) function.data)->ret_type;
}


static Type * get_param_types(Type function) {
    assert_eq(function.class, TYPE_FUNCTION);
    return (Type *) ((void *) function.data + sizeof(FunctionTypeData));
}


static u32 get_param_count(Type function) {
    assert_eq(function.class, TYPE_FUNCTION);
    return ((FunctionTypeData *) function.data)->param_count;
}


static bool is_type_incomplete(Type type) {
    if (type.class == TYPE_UNKNOWN) return 1;
    if (type.class == TYPE_FUNCTION) {
        u32 param_count = get_param_count(type);
        if (is_type_incomplete(*get_ret_type(type))) return 1;
        for (u32 i = 0; i < param_count; i++)
            if (is_type_incomplete(get_param_types(type)[i]))
                return 1;
        return 0;
    }
    assert(is_type_primitive(type.class));
    return 0;
}


/* DANGEROUS: Allocates space for the parameter types, but the caller is
 * expected to add them.
 */
static Type make_type_function(Type ret_type, u32 param_count) {
    FunctionTypeData data = {
        .ret_type = ret_type,
        .param_count = param_count,
    };
    void * ptr = aalloc(
        parser_arena,
        sizeof(FunctionTypeData) + data.param_count * sizeof(Type)
    );
    *(FunctionTypeData *)ptr = data;
    return (Type) {
        .class = TYPE_FUNCTION,
        .data = ptr
    };
}

#define ptype(c) ((Type) { .class = (c) })

static Type make_ptype(TypeClass class) {
    assert(is_type_primitive(class));
    return ptype(class);
}


static Type make_type_unknown() { return ptype(TYPE_UNKNOWN); }


static bool type_eq(const Type * a, const Type * b) {

    if (a->class != b->class) return 0;

    if (is_type_primitive(a->class)) return 1;
    if (a == b) return 1;

    switch ((int) a->class) {
        case TYPE_FUNCTION: {
            u32 alen = get_param_count(*a);
            u32 blen = get_param_count(*b);
            if (alen != blen) return 0;
            Type * atypes = get_param_types(*a);
            Type * btypes = get_param_types(*b);
            for (u32 i = 0; i < alen; i++) {
                if (!type_eq(&atypes[i], &btypes[i]))
                    return 0;
            }
            return 1;
        }
    }
    unreachable return 0;
}



/* Expression Functions
 * -----------------------------------------------------------------------------
 */

#define MAKE_EXPR(class_val, inner_var_ptr, inner_type, ...) \
    (((inner_var_ptr) = aalloc(code_arena, sizeof(inner_type))), \
    (*(inner_var_ptr) = (inner_type) __VA_ARGS__), \
    (Expr) { \
        .class = (class_val), \
        .expr = (inner_var_ptr) \
    }) \


static Expr make_expr_function_literal(Function fn) {
    Function * _; return MAKE_EXPR(FN_LITERAL, _, Function, fn);
}

static Expr make_expr_int_literal(Integer val) {
    Integer * _; return MAKE_EXPR(INT_LITERAL, _, Integer, val);
}


static Expr make_expr_str_literal(slice_t from) {
    String str = { .chars = aalloc(code_arena, slicelen(from)), .len = slicelen(from) };
    memcpy(str.chars, from.sptr, slicelen(from));
    String * _;
    return MAKE_EXPR(STR_LITERAL, _, String, str);
}


static Expr make_expr_builtin(
    BuiltinClass class, OpBuiltin ** op_ptr
) {
    return MAKE_EXPR(OP_BUILTIN, *op_ptr, OpBuiltin, { .class = class });
}

static Expr make_expr_call(Expr fn, OpCall ** op_ptr) {
    return MAKE_EXPR(OP_CALL, *op_ptr, OpCall, { .fn = fn });
}


static Expr make_expr_var(Ident ident, TypeClass type_class) {
    assert(type_class != TYPE_NONE);
    assert(type_class != TYPE_UNKNOWN);
    OpVar * _;
    return MAKE_EXPR(OP_VAR, _, OpVar, { .ident = ident, .size = sizeof_type(type_class) });
}


static Expr make_expr_assignment(Expr val, Ident ident, TypeClass type_class) {
    assert(type_class != TYPE_NONE);
    assert(type_class != TYPE_UNKNOWN);
    OpAssign * _; return MAKE_EXPR
        (OP_ASSIGN, _, OpAssign, { ident, val, sizeof_type(type_class)});
}


static Expr make_expr_if(Expr cond, Expr if_expr) {
    OpIf * _; return MAKE_EXPR(OP_IF, _, OpIf, { cond, if_expr });
}


static Expr make_expr_if_else(Expr cond, Expr if_expr, Expr else_expr) {
    OpIfElse * _;
    return MAKE_EXPR(OP_IF_ELSE, _, OpIfElse, { cond, if_expr, else_expr });
}


static Expr make_expr_while(Expr cond, Expr while_expr) {
    OpIf * _; return MAKE_EXPR(OP_WHILE, _, OpIf, { cond, while_expr });
}


static Expr make_expr_seq(Expr * exprs, u32 expr_count) {
    OpSeq * _; return MAKE_EXPR(OP_SEQ, _, OpSeq, { exprs, expr_count });
}



/* Definition Functions
 * -----------------------------------------------------------------------------
 */

static Ident add_var(Context * ctx, slice_t name, Type type) {
    u32 offset = ctx->next_offset, index = ctx->next_index;
    VarLocation loc = (ctx->locals == ctx->globals)? GLOBAL : LOCAL;

    Ident ident = ident_new(loc, offset);

    Def def = { .name = name, .type = type, .ident = ident };

    if (index >= ctx->locals->len) {
        assert_eq(index, ctx->locals->len);
        da_append(*ctx->locals, def);
    } else {
        assert_eq(ctx->locals->items[index].ident, def.ident);
        ctx->locals->items[index] = def;
    }

    ctx->next_index++;
    ctx->next_offset = offset + sizeof_type(type.class);
    return ident;
}


#define vars_size(ctx) \
    ((ctx).next_offset)



/* String Parsing Functions
 * -----------------------------------------------------------------------------
 */

typedef struct {
    char * name;
    struct {
        char * params_sh;
        u32 class;
    } variants[4];
} BuiltinProto;


static const BuiltinProto builtin_protos[] = {
    "+",     { "ii", B_ADD_I, "vi", B_ADD_VI,},
    "*",     { "ii", B_MUL_I, "vi", B_MUL_VI },
    "-",     { "ii", B_SUB_I },
    "/",     { "ii", B_DIV_I },
    "print", { "i",  B_PRINT_I, "s", B_PRINT_S },
};


static const Type builtin_types[] = {
    [B_ADD_I]    = ptype(TYPE_INT),
    [B_SUB_I]    = ptype(TYPE_INT),
    [B_MUL_I]    = ptype(TYPE_INT),
    [B_DIV_I]    = ptype(TYPE_INT),
    [B_ADD_VI]   = ptype(TYPE_INT),
    [B_MUL_VI]   = ptype(TYPE_INT),
    [B_PRINT_I]  = ptype(TYPE_VOID),
    [B_PRINT_S]  = ptype(TYPE_VOID)
};


static Type get_builtin_type(BuiltinClass class) {
    assert(class < arrlen(builtin_types));
    Type type = builtin_types[class];
    assert(type.class != TYPE_NONE);
    return type;
}


static u32 match_type_sh(Type type, const char * sh) {
    assert(type.class != TYPE_UNKNOWN);
    char exsh = type_shorthands[type.class];
    if (*sh != exsh) return 0;
    if (is_type_primitive(type.class)) return 1;

    switch (type.class) {
        case TYPE_FUNCTION:
            {
                u32 i, param_count = sh[1];
                if (get_param_count(type) != param_count) return 0;
                for (i = 0; i < param_count; i++)
                    if (!match_type_sh(get_param_types(type)[i], sh + i + 1)) return 0;

                return i + 1;
            }
        default: panic();
    }
}


static bool match_param_types_sh(const char * sh, const Type * types, u32 count) {
    const char * vtype = NULL;
    for (u32 i = 0; i < count; i++) {
        if (*sh == 'v') vtype = sh + 1;
        u32 n;
        if (vtype)
            n = match_type_sh(types[i], vtype);
        else {
            n = match_type_sh(types[i], sh);
            sh += n;
        }
        if (!n) return 0;
    }
    return (vtype) || *sh == 0;
}


static bool is_builtin(const slice_t name, u32 * out_index) {
    for (u32 i = 0; i < arrlen(builtin_protos); i++)
        if (slice_str_eq(name, builtin_protos[i].name)) {
            if (out_index) *out_index = i;
            return 1;
        }

    return 0;
}


static u32 get_builtin_class(u32 index, const Type * param_types, u32 param_count) {
    const BuiltinProto * proto = &builtin_protos[index];
    char * type_sh;
    for (u32 i = 0; (type_sh = proto->variants[i].params_sh); i++)
        if (match_param_types_sh(type_sh, param_types, param_count))
            return proto->variants[i].class;

    return B_NONE; 
}


static Integer lstr_to_int(slice_t str) {
    char * end_ptr;
    Integer ret = strtol(str.sptr, &end_ptr, 10); 
    assert_eq(end_ptr, str.eptr);
    return ret;
}



/* Token Parsing Functions
 * -----------------------------------------------------------------------------
 */

#define skip(token_ptr, token_class) do { \
    assert_eq((token_ptr)->class, (token_class)); \
    (token_ptr)++; \
} while (0);

#define skip_expect(token_ptr, token_class, ...) \
    if ((token_ptr)++->class != (token_class)) error((token_ptr), __VA_ARGS__)


/* parses a type and moves to first token after the type. if there is no type,
 * returns TYPE_NONE and does not move.
 */
static Type parse_type(const Token ** tokens) {
    TypeClass class;
    if ((*tokens)->class == TK_IDENT)
        class = str_to_type_class((*tokens)->val);
    else class = TYPE_NONE;

    Type type = make_ptype(class);
    assert(is_type_primitive(class));

    if (class == TYPE_NONE) return type;
    (*tokens)++;
    return type;
}


static void parse_expr(
    const Token ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
);


/* Call needs to know the offsets for each parameter. they can be calculated
 * here, but it would be nice if this function could access the local defs of
 * the called function to ensure the offsets are the same.
 */
static void parse_call_params(
    const Token ** tokens, Context * context,
    Type * expected_param_types, u32 expected_param_count,
    Expr ** out_params, u32 ** out_param_offsets, u32 * out_param_count
) {
    Expr * params = aalloc(code_arena, expected_param_count * sizeof(Expr));
    u32 * offsets = aalloc(code_arena, expected_param_count * sizeof(u32));

    assert(delim_value(*tokens) > 0);
    TkClass close = opposite_delim((*tokens)->class);
    (*tokens)++;

    u32 offs = 0, count = 0;
    for(;
        (*tokens)->class != close
        && (*tokens)->class != TK_EOF
        && (count < expected_param_count);
        count++
    ) {
        Type param_type;
        const Token * param_token = *tokens;
        parse_expr(tokens, context, &params[count], &param_type);
        if (!type_eq(&param_type, &expected_param_types[count]))
            error(param_token, "mismatched parameter types");
        offsets[count] = offs;
        offs += sizeof_type(param_type.class);
    }

    if (count != expected_param_count)
        error(*tokens + 2, "incorrect number of parameters provided");

    *out_params = params;
    *out_param_offsets = offsets;
    *out_param_count = count;
}


static void parse_pseudo_list(
    Token const ** tokens, Context * context, TkClass open_delim,
    Expr ** out_items, Type ** out_types, u32 * out_count
) {
    skip_expect(*tokens, open_delim, "expected opening bracket/parenthesis");

    TkClass close_delim = opposite_delim(open_delim);

    da(Expr) params = {};
    da(Type) param_types = {};


    u32 i = 0;
    for(; (*tokens)->class != close_delim && (*tokens)->class != TK_EOF; i++) {
        Expr param;
        Type param_type;
        parse_expr(tokens, context, &param, &param_type);
        da_append(params, param);
        da_append(param_types, param_type);
    }

    skip_expect(*tokens, close_delim, "expected closing bracket/parenthesis");

    *out_items = aalloc(code_arena, i * sizeof(Expr));
    *out_types = aalloc(parser_arena, i * sizeof(Type));
    *out_count = i;

    memcpy(*out_items, params.items, i * sizeof(Expr));
    memcpy(*out_types, param_types.items, i * sizeof(Type));

    da_dealloc(params);
    da_dealloc(param_types);
}


static bool find_name(slice_t name, const Def * defs, u32 defs_len, u32 * out_index) {
    for (*out_index = 0; *out_index < defs_len; (*out_index)++)
        if (slice_eq(defs[*out_index].name, name)) return true;
    return false;
}


/* temporary hack warning: */
static Def * get_def(slice_t name, Context context) {
    u32 var_index;
    if (find_name(name, context.globals->items, context.globals->len, &var_index))
        return &context.globals->items[var_index];
    else if (find_name(name, context.locals->items, context.locals->len, &var_index))
        return &context.locals->items[var_index];
    else return NULL;
}


static void parse_expr_builtin(
    const Token ** tokens, Context * context, int index,
    Expr * out_expr, Type * out_ret_type
) {
    skip(*tokens, TK_IDENT);
 
    const Token * params_start = *tokens;

    Type * param_types;
    Expr * params;
    u32 param_count;
    parse_pseudo_list(
        tokens, context, TK_OPEN, &params, &param_types, &param_count
    );
    
    u32 class = get_builtin_class(index, param_types, param_count);
    if (class == B_NONE) error(params_start, "incorrect parameters");
    
    Type ret_type = get_builtin_type(class);
    
    OpBuiltin * op;
    *out_expr = make_expr_builtin(class, &op);
    op->args = params;
    op->args_len = param_count;

    *out_ret_type = ret_type;
}


static void parse_expr_seq(
    const Token ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
) {
    Expr * exprs;
    Type * expr_types;
    u32 expr_count;

    parse_pseudo_list(
        tokens, context, TK_CB_OPEN, &exprs, &expr_types, &expr_count
    );

    Type ret_type;
    if (expr_count)
        ret_type = expr_types[expr_count - 1];
    else
        ret_type = make_ptype(TYPE_VOID);

    *out_ret_type = ret_type;
    *out_expr = make_expr_seq(exprs, expr_count);
}


static void parse_expr_call(
    const Token ** tokens, Context * context, Expr fn_expr, Type fn_type,
    Expr * out_expr, Type * out_ret_type
) {
    OpCall * call;
    *out_expr = make_expr_call(fn_expr, &call);
 
    skip(*tokens, TK_IDENT);
    parse_call_params(
        tokens, context, get_param_types(fn_type), get_param_count(fn_type),
        &call->params, &call->param_offsets, &call->param_count
    ); 

    *out_ret_type = *get_ret_type(fn_type);
    skip(*tokens, TK_CLOSE);
}


static void parse_if(
    const Token ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
) {
    skip(*tokens, TK_IDENT);

    Expr cond_expr, if_expr;
    Type cond_type, if_type;
    const Token * cond_token = *tokens, * if_token;
    parse_expr(tokens, context, &cond_expr, &cond_type);

    if (cond_type.class != TYPE_INT) error(cond_token, "expected int");
    
    if_token = *tokens;
    parse_expr(tokens, context, &if_expr, &if_type);

    if ((*tokens)->class == TK_IDENT && slice_str_eq((*tokens)->val, "else")) {
        skip(*tokens, TK_IDENT);
        Expr else_expr;
        Type else_type;

        parse_expr(tokens, context, &else_expr, &else_type);

        if (!type_eq(&if_type, &else_type))
            error(if_token, "expected if and else expressions to be the same type");

        *out_expr = make_expr_if_else(cond_expr, if_expr, else_expr);
        *out_ret_type = if_type;
    } else {
        *out_expr = make_expr_if(cond_expr, if_expr);
        *out_ret_type = make_ptype(TYPE_VOID);
    }

}


static void parse_while(
    const Token ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
) {
    skip(*tokens, TK_IDENT);
    Expr cond_expr, while_expr;
    Type cond_type, while_type;
    
    const Token * cond_token = *tokens;
    parse_expr(tokens, context, &cond_expr, &cond_type);

    if (cond_type.class != TYPE_INT) error(cond_token, "expected int");
    
    parse_expr(tokens, context, &while_expr, &while_type);

    *out_expr = make_expr_while(cond_expr, while_expr);
    *out_ret_type = make_ptype(TYPE_VOID);
}


static void parse_ident(
    const Token ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
) {
    slice_t name = (*tokens)->val;

    if (slice_str_eq(name, "if"))
        return parse_if(tokens, context, out_expr, out_ret_type);
    if (slice_str_eq(name, "while"))
        return parse_while(tokens, context, out_expr, out_ret_type);

    { u32 index; if (is_builtin((*tokens)->val, &index))
        return parse_expr_builtin(
            tokens, context, index, out_expr, out_ret_type
        );
    }


    Def * def = get_def(name, *context);

    if (!def)
        error(*tokens, "undeclared identifier");

    if (is_type_incomplete(def->type))
        error(*tokens,
            "incomplete type. move this identifier's declaration above its "
            "usage or add a type annotation");

    Expr var_expr = make_expr_var(def->ident, def->type.class);

    bool is_call = (*tokens + 1)->class == TK_OPEN;

    if (is_call) {
        if (def->type.class != TYPE_FUNCTION) error(*tokens, "not a function");

        parse_expr_call(
            tokens, context, var_expr, def->type, out_expr, out_ret_type
        );
    } else {
        *out_expr = var_expr;
        *out_ret_type = def->type;
        skip(*tokens, TK_IDENT);
    }
}


static void parse_expr_assign(
    const Token ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
) {
    skip(*tokens, TK_ASSIGN);

    if ((*tokens)->class != TK_IDENT)
        error(*tokens, "expected identifier");

    const Token * var_token = *tokens;
    const slice_t var_name = var_token->val;

    skip(*tokens, TK_IDENT);

    Expr val;
    Type val_type;
    parse_expr(tokens, context, &val, &val_type);


    Def * var_def = get_def(var_name, *context);
    
    Type var_type;
    Ident var_ident;

    if (var_def) {
        var_type = var_def->type;
        var_ident = var_def->ident;
        
        if (is_type_incomplete(var_type))
            var_def->type = val_type;

        if (!type_eq(&var_type, &val_type))
            error(var_token, "mismatched types");
    } else {
        if (is_builtin(var_name, NULL))
            error(var_token, "not a valid identifier");

        var_type = val_type;
        var_ident = add_var(context, var_name, var_type);
    }

    *out_expr = make_expr_assignment(val, var_ident, var_type.class);
    *out_ret_type = var_type;
}


/* to know how much space to allocate for the params, we parse them twice.
 * This function will error on invalid param list, so no need to check that
 * again
 */
static u32 parser_count_params(const Token * open) {
    const Token * token = open;
    skip_expect(token, TK_OPEN, "expected parameter list `(...)`");
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


static void parse_expr_function_lit(
    const Token ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
) {

    Type expected_fn_ret_type = make_ptype(TYPE_NONE);

    skip(*tokens, TK_FUNCTION);
    skip_expect(*tokens, TK_ARROW, "expected `-> <return-type>`");
    const Token * ret_type_token = *tokens;
    expected_fn_ret_type = parse_type(tokens);
    if (expected_fn_ret_type.class == TYPE_NONE) error(*tokens, "expected type");

    u32 param_count = parser_count_params(*tokens);

    DefList locals = da_new(Def, param_count);
    Context ctx = { .globals = context->globals, .locals = &locals };
    
    Type type = make_type_function(make_ptype(TYPE_NONE), param_count);

    skip(*tokens, TK_OPEN);
    for (u32 i = 0; i < param_count; i++) {
        Type param_type = parse_type(tokens);
        get_param_types(type)[i] = param_type;
        add_var(&ctx, (*tokens)->val, param_type);
        skip(*tokens, TK_IDENT);
    }
    skip(*tokens, TK_CLOSE);

    Function fn;

    Type fn_ret_type;
    parse_expr(tokens, &ctx, &fn.body, &fn_ret_type);
    
    if (!type_eq(&fn_ret_type, &expected_fn_ret_type))
        error(ret_type_token, "mismatched return type");

    *get_ret_type(type) = fn_ret_type;

    fn.stack_size = vars_size(ctx);

    da_dealloc(locals);

    *out_expr = make_expr_function_literal(fn);
    *out_ret_type = type;
}


static void parse_expr(
    const Token ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
) {
    switch ((*tokens)->class) {
        case TK_INT:
            *out_ret_type = make_ptype(TYPE_INT);
            *out_expr = make_expr_int_literal(lstr_to_int((*tokens)->val));
            (*tokens)++;
            break;
        case TK_STR:
            *out_ret_type = make_ptype(TYPE_STR);
            *out_expr = make_expr_str_literal((*tokens)->val);
            (*tokens)++;
            break;
        case TK_FUNCTION:
            parse_expr_function_lit(tokens, context, out_expr, out_ret_type);
            break;
        case TK_CB_OPEN:
            parse_expr_seq(tokens, context, out_expr, out_ret_type);
            break;
        case TK_IDENT:
            parse_ident(tokens, context, out_expr, out_ret_type);
            break;
        case TK_ASSIGN:
            parse_expr_assign(tokens, context, out_expr, out_ret_type);
            break;
        default: error(*tokens, "expected expression");
    }
}


/* Tries to get the type of an expression without fully parsing the expression.
 * If there's not enough context, returns TYPE_UNKNOWN. Either way, does its
 * best to move *tokens to the first token after the end of the expression.
 */
static Type predef_parse_expr_type(const Token ** tokens) {
    switch ((*tokens)->class) {
        case TK_INT:
            skip(*tokens, TK_INT);
            return make_ptype(TYPE_INT);
        case TK_STR:
            skip(*tokens, TK_STR);
            return make_ptype(TYPE_STR);
        case TK_FUNCTION:
            {
                Type type, ret_type;
                skip(*tokens, TK_FUNCTION);

                if ((*tokens)->class == TK_ARROW) {
                    skip(*tokens, TK_ARROW);
                    ret_type = parse_type(tokens);
                    if (ret_type.class == TYPE_NONE)
                        error(*tokens, "expected type");
                } else ret_type = make_type_unknown();

                u32 param_count = parser_count_params(*tokens);

                type = make_type_function(ret_type, param_count); 

                skip(*tokens, TK_OPEN);
                for (u32 i = 0; i < param_count; i++) {
                    get_param_types(type)[i] = parse_type(tokens);
                    skip(*tokens, TK_IDENT);
                }
                skip(*tokens, TK_CLOSE);

                skip_to_end(tokens);
                return type;
            }
        default:
            skip_to_end(tokens);
            return make_type_unknown();
    }
}


static void predef_parse_global_decl(const Token ** tokens, Context * context) {
    skip_expect(*tokens, TK_ASSIGN, "expected declaration");

    if ((*tokens)->class != TK_IDENT)
        error(*tokens, "expected identifier");

    const Token * var_token = *tokens;
    const slice_t var_name = var_token->val;

    skip(*tokens, TK_IDENT);

    const Token * initializer_token = *tokens;

    Type type = predef_parse_expr_type(tokens);

    if (type.class == TYPE_UNKNOWN || type.class == TYPE_NONE)
        error(initializer_token, "can't determine type");

    add_var(context, var_name, type);
}


static Context predefine_globals(const Token * tokens) {
    DefList * globals = aalloc(parser_arena, sizeof(DefList));
    *globals = (DefList) {};
    Context ctx = { .globals = globals, .locals = globals };

    while (tokens->class != TK_EOF)
        predef_parse_global_decl(&tokens, &ctx);

    return ctx;
}


static AST parse_tokens(const Token * tokens) {
    Context ctx = predefine_globals(tokens);

    AST ast = { .arena = arena_init() };

    /* global variable pointer to stack variable. what could go wrong */
    code_arena = &ast.arena;

    ast.global_count = ctx.globals->len;
    ast.global_decls = aalloc(code_arena, ast.global_count * sizeof(Expr));

    for (u32 i = 0; i < ast.global_count; i++) {
        Type type;
        parse_expr_assign(&tokens, &ctx, &ast.global_decls[i], &type);
    }

    ast.globals_size = vars_size(ctx);

    Def * main_def;

    if (!(main_def = get_def(slice("main"), ctx)))
        panic("no main function");

    if (main_def->type.class != TYPE_FUNCTION)
        panic("main isn't a function");

    ast.main_returns_exit_code = (get_ret_type(main_def->type)->class == TYPE_INT);

    ast.main = main_def->ident;

    free(ctx.globals->items);

    return ast;
}

/* API Functions
 * -----------------------------------------------------------------------------
 */

void free_code(AST ast) { afree(ast.arena); }


AST parse_code(const char * code) {
    parser_arena = malloc(sizeof(Arena));
    *parser_arena = arena_init();

    const Token * tokens = tokenize(code, parser_arena);
    AST ast = parse_tokens(tokens);
    //parse_tokens(&tokens, &ast.globals, &ast.main_fn, &ast.ignore_ret);

    afree(*parser_arena);
    free(parser_arena);

    return ast;
}
