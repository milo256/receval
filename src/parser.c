#include "parser.h"

#include "common.h"
#include "expr.h"
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



/* Receval Types
 * -----------------------------------------------------------------------------
 */

#define DEF_PRIMITIVE_TYPES(f) \
/*  | enum name | str name | shorthand | size */\
    f(TYPE_VOID,  "void",    'n',        0) \
    f(TYPE_INT,   "int",     'i',        sizeof(Integer)) \
    f(TYPE_STR,   "str",     's',        sizeof(String))

#define DEF_COMPLEX_TYPES(f) \
/*  | enum name      | str name | shorthand | size */\
    f(TYPE_FUNCTION,   NULL,      'p',        sizeof(int *))

#define DEF_TYPES(f) \
    DEF_PRIMITIVE_TYPES(f) DEF_COMPLEX_TYPES(f)

#define TC_ENUM(en, sn, sh, sz) en,
#define TYPE_SIZE_TABLE(en, sn, sh, sz) [en] = sz,
#define TYPE_NAME_TABLE(en, sn, sh, sz) [en] = sn,
#define TYPE_SH_TABLE(en, sn, sh, sz) [en] = sh,

typedef enum {
    TYPE_NONE,
    DEF_PRIMITIVE_TYPES(TC_ENUM)
    FIRST_COMPLEX_TYPE,
    LAST_PRIMITIVE_TYPE = FIRST_COMPLEX_TYPE - 1,
    DEF_COMPLEX_TYPES(TC_ENUM)
    NTYPES
} TypeClass;


static u32 type_sizes[] = { DEF_TYPES(TYPE_SIZE_TABLE) };
static char * type_names[] = { DEF_TYPES(TYPE_NAME_TABLE) };
static char type_shorthands[] = { DEF_TYPES(TYPE_SH_TABLE) };


typedef struct Type {
    TypeClass class;
    void * data;
} Type;


typedef struct {
    Type ret_type;
    u32 param_count;    
    /* and then args array is stored immediately after this in memory (trust) */
}  FunctionTypeData;



/* Definition Types
 * -----------------------------------------------------------------------------
 */

/* we don't need ident because ident is just offset + location, and location is
 * which array the def is stored in */
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

static void error(Token const * tk, char * msg) {
    fprintf(
        stderr, "Receval: Error on %d:%d: %s\n",
        tk->dbug_line, tk->dbug_column, msg
    );
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


static u32 opposite_delim(u32 class) {
    return (class % 2)? class + 1 : class - 1;
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
    for (u32 i = 0; i < ARRLEN(type_names); i++) {
        if (!type_names[i]) continue;
        if (lstr_str_eq(str, type_names[i])) return i;
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
    ASSERT(type_class != TYPE_NONE);
    u32 size = type_sizes[type_class];
    ASSERT(size <= TYPE_MAX_SIZE);
    return size;
}


static Type * get_ret_type(Type function) {
    ASSERT(function.class == TYPE_FUNCTION);
    return &((FunctionTypeData *) function.data)->ret_type;
}


static Type * get_param_types(Type function) {
    ASSERT(function.class == TYPE_FUNCTION);
    return (Type *) ((void *) function.data + sizeof(FunctionTypeData));
}


static u32 get_param_count(Type function) {
    ASSERT(function.class == TYPE_FUNCTION);
    return ((FunctionTypeData *) function.data)->param_count;
}


static Type make_type_fn_ptr(Type ret_type, u32 param_count) {
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
    ASSERT(is_type_primitive(class));
    return ptype(class);
}


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

#define MAKE_EXPR(class_val, ret_size_val, inner_var_ptr, inner_type, ...) \
    (((inner_var_ptr) = aalloc(code_arena, sizeof(inner_type))), \
    (*(inner_var_ptr) = (inner_type) __VA_ARGS__), \
    (Expr) { \
        .class = (class_val), \
        .ret_size = (ret_size_val), \
        .expr = (inner_var_ptr) \
    }) \


static Expr make_expr_int_literal(Integer val) {
    Integer * _; return MAKE_EXPR(INT_LITERAL, sizeof(val), _, Integer, val);
}


static Expr make_expr_str_literal(LStr from) {
    String str = { .chars = aalloc(code_arena, from.len), .len = from.len };
    memcpy(str.chars, from.chars, from.len);
    String * _;
    return MAKE_EXPR(STR_LITERAL, sizeof(String), _, String, str);
}


static Expr make_expr_builtin(
    BuiltinClass class, TypeClass ret_type_class, OpBuiltin ** op_ptr
) {
    return MAKE_EXPR(
        OP_BUILTIN, sizeof_type(ret_type_class),*op_ptr,
        OpBuiltin, { .class = class }
    );
}


static Expr make_expr_call(Expr fn, TypeClass ret_type_class, OpCall ** op_ptr) {
    return MAKE_EXPR
        (OP_CALL, sizeof_type(ret_type_class), *op_ptr, OpCall, { .fn = fn });
}


static Expr make_expr_var(Ident ident, TypeClass type_class) {
    OpVar * _;
    return MAKE_EXPR(OP_VAR, sizeof_type(type_class), _, OpVar, { ident });
}


static Expr make_expr_assignment(Expr val, Ident ident, TypeClass type_class) {
    OpAssign * _; return MAKE_EXPR
        (OP_ASSIGN, sizeof_type(type_class), _, OpAssign, { ident, val });
}


static Expr make_expr_if(Expr cond, Expr if_expr) {
    OpIf * _; return MAKE_EXPR(OP_IF, 0, _, OpIf, { cond, if_expr });
}


static Expr make_expr_if_else(Expr cond, Expr if_expr, Expr else_expr) {
    OpIfElse * _;
    return MAKE_EXPR(
        OP_IF_ELSE, MAX(if_expr.ret_size, else_expr.ret_size),
        _, OpIfElse, { cond, if_expr, else_expr }
    );
}


static Expr make_expr_while(Expr cond, Expr while_expr) {
    OpIf * _; return MAKE_EXPR(OP_WHILE, 0, _, OpIf, { cond, while_expr });
}


static Expr make_expr_seq(Expr * exprs, u32 expr_count, TypeClass ret_type_class) {
    OpSeq * _; return MAKE_EXPR
        (OP_SEQ, sizeof_type(ret_type_class), _, OpSeq, { exprs, expr_count });
}



/* Definition Functions
 * -----------------------------------------------------------------------------
 */

static Ident add_local(Context * ctx, LStr name, Type type) {
    u32 offset = ctx->locals.len ?
        ctx->locals.items[ctx->locals.len - 1].offset
        + sizeof_type(ctx->locals.items[ctx->locals.len - 1].type.class) : 0;

    Ident ident = ident_new(LOCAL, offset);

    Def def = {
        .name = name, .type = type,
        .offset = var_offset(ident)
    };

    da_append(ctx->locals, def);
    return ident;
}


static void create_param_defs(const Def * function_def, Def * buf) {
    ASSERT(function_def->type.class == TYPE_FUNCTION);
    u32 offset = 0,
        param_count = get_param_count(function_def->type);

    Type * types = get_param_types(function_def->type);
    LStr * names = function_def->param_names;
    
    for (u32 i = 0; i < param_count; i++) {
        buf[i] = (Def) {
            .name = names[i], .type = types[i],
            .offset = offset, .init = NULL,
        };
        offset += sizeof_type(types[i].class);
    } 
}


#define defs_size(list) \
        ((list).len ? (list).items[(list).len - 1].offset + sizeof_type((list).items[(list).len - 1].type.class) : 0)



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
    ASSERT(class < ARRLEN(builtin_types));
    Type type = builtin_types[class];
    ASSERT(type.class != TYPE_NONE);
    return type;
}


static u32 match_type_sh(Type type, const char * sh) {
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
        default: PANIC();
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


static bool is_builtin(const LStr name, u32 * out_index) {
    for (u32 i = 0; i < ARRLEN(builtin_protos); i++)
        if (lstr_str_eq(name, builtin_protos[i].name)) {
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


static Integer lstr_to_int(LStr str) {
    char * end_ptr;
    Integer ret = strtol(str.chars, &end_ptr, 10); 
    ASSERT(end_ptr == str.chars + str.len);
    return ret;
}



/* Token Parsing Functions
 * -----------------------------------------------------------------------------
 */

#define skip(token_ptr, token_class) ASSERT((token_ptr)++->class == (token_class))

#define skip_expect(token_ptr, token_class, ...) \
    if ((token_ptr)++->class != (token_class)) error((token_ptr), __VA_ARGS__)


/* TODO: implement :P */
static Type parse_type(const Token ** token) {
    Type ret = { lstr_to_type_class((*token)->val), NULL };
    (*token)++;
    return ret;
}


static void parse_expr(
    const Token ** tokens, Context * context,
    Expr * out_expr, Type * out_ret_type
);


static void parse_call_params(
    const Token ** tokens, Context * context,
    Type * expected_param_types, u32 expected_param_count,
    Expr ** out_params, u32 * out_param_count
) {
    Expr * params = aalloc(code_arena, expected_param_count * sizeof(Expr));

    ASSERT(delim_value(*tokens) > 0);
    TkClass close = opposite_delim((*tokens)->class);
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
    Type ret_type = get_builtin_type(class);

    if (class == B_NONE) error(params_start, "incorrect parameters");
    
    OpBuiltin * op;
    *out_expr = make_expr_builtin(class, ret_type.class, &op);
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
    *out_expr = make_expr_seq(exprs, expr_count, ret_type.class);
}


static void parse_expr_call(
    const Token ** tokens, Context * context, Expr fn_expr, Type fn_type,
    Expr * out_expr, Type * out_ret_type
) {
    OpCall * call;
    *out_expr = make_expr_call(fn_expr, get_ret_type(fn_type)->class, &call);
 
    skip(*tokens, TK_IDENT);
    parse_call_params(
        tokens, context, get_param_types(fn_type), get_param_count(fn_type),
        &call->args, &call->args_len
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

    if ((*tokens)->class == TK_IDENT && lstr_str_eq((*tokens)->val, "else")) {
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
    LStr name = (*tokens)->val;

    if (lstr_str_eq(name, "if"))
        return parse_if(tokens, context, out_expr, out_ret_type);
    if (lstr_str_eq(name, "while"))
        return parse_while(tokens, context, out_expr, out_ret_type);

    { u32 index; if (is_builtin((*tokens)->val, &index))
        return parse_expr_builtin(
            tokens, context, index, out_expr, out_ret_type
        );
    }


    Type var_type;
    Ident ident;

    if (!parse_var(name, *context, &var_type, &ident))
        error(*tokens, "unknown identifier");

    Expr var_expr = make_expr_var(ident, var_type.class);

    bool is_call = (*tokens + 1)->class == TK_OPEN;

    if (is_call) {
        if (var_type.class != TYPE_FUNCTION) error(*tokens, "not a function");

        parse_expr_call(
            tokens, context, var_expr, var_type, out_expr, out_ret_type
        );
    } else {
        *out_expr = var_expr;
        *out_ret_type = var_type;
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
    const LStr var_name = var_token->val;

    skip(*tokens, TK_IDENT);

    Expr val;
    Type val_type;
    parse_expr(tokens, context, &val, &val_type);

    Type var_type;
    Ident var_ident;

    const bool var_exists = parse_var(var_name, *context, &var_type, &var_ident);

    if (var_exists) {
        if (!type_eq(&var_type, &val_type))
            error(var_token, "mismatched types");
    } else {
        if (is_builtin(var_name, NULL))
            error(var_token, "not a valid identifier");

        var_type = val_type;
        var_ident = add_local(context, var_name, var_type);
    }

    *out_expr = make_expr_assignment(val, var_ident, var_type.class);
    *out_ret_type = var_type;
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
        case TK_CB_OPEN:
            parse_expr_seq(tokens, context, out_expr, out_ret_type);
            break;
        case TK_IDENT:
            parse_ident(tokens, context, out_expr, out_ret_type);
            break;
        case TK_ASSIGN: {
            parse_expr_assign(tokens, context, out_expr, out_ret_type);
            break;
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
        skip(token, TK_FUNCTION);
        skip_expect(token, TK_ARROW, "expected return type `-> <type>`");
        
        const Token * ret_type_token = token;
        Type ret_type = parse_type(&token);
        if (ret_type.class == TYPE_NONE) error(ret_type_token, "expected type");

        u32 param_count = parser_count_params(token);
        skip(token, TK_OPEN);
        
        param_names = aalloc(code_arena, sizeof(LStr) * param_count);

        type = make_type_fn_ptr(ret_type, param_count); 
        for (u32 i = 0; i < param_count; i++) {
            get_param_types(type)[i] = parse_type(&token);
            param_names[i] = token++->val;
        }
        skip(token, TK_CLOSE);
        init_token = token;
        skip_to_end(&token);
    } else {
        if (token->class != TK_INT) error(token, "value expected for global definition");
        type = (Type) { .class = TYPE_INT };
        init_token = token;
        skip(token, TK_INT);
    }
    
    Def def = (Def) {
        .name = var_name, .offset = defs_size(*globals),
        .type = type, .init = (Token *) init_token, .param_names = param_names
    };
    da_append(*globals, def);
    *tokens = token;
}


static void parse_tokens(
    const Token ** tokens, void ** out_globals, Function ** out_main,
    bool * out_ignore_ret
) {
    DefList global_defs = {};
    *out_main = NULL;

    while((*tokens)->class != TK_EOF)
        parse_global_def(tokens, &global_defs);

    void * global_values = aalloc(code_arena, defs_size(global_defs));

    for (u32 i = 0; i < global_defs.len; i++) {
        Def * def = &global_defs.items[i];
        switch (def->type.class) {
            case TYPE_FUNCTION: {
                Function * fn = aalloc(code_arena, sizeof(Function));
                *fn = parse_function_code(
                    def, global_defs.items, global_defs.len, NULL
                ); 
                *out_ignore_ret = (get_ret_type(def->type)->class != TYPE_INT);
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

void free_code(AST ast) { afree(ast.arena); }


AST parse_code(const char * code) {
    AST ast = { .arena = arena_init() };
    parser_arena = malloc(sizeof(Arena));
    *parser_arena = arena_init();
    code_arena = &ast.arena;

    const Token * tokens = tokenize(code, code_arena);
    parse_tokens(&tokens, &ast.globals, &ast.main_fn, &ast.ignore_ret);

    afree(*parser_arena);
    free(parser_arena);

    return ast;
}
