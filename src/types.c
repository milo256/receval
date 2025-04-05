#include "types.h"

static u32 type_sizes[] = { DEF_TYPES(TYPE_SIZE_TABLE) };
static char * type_names[] = { DEF_TYPES(TYPE_NAME_TABLE) };
static char type_shorthands[] = { DEF_TYPES(TYPE_SH_TABLE) };

bool is_type_primitive(TypeClass class) {
    return (class < FIRST_COMPLEX_TYPE);
}


u32 sizeof_type(int type_class) {
    assert(type_class != TYPE_NONE);
    assert(type_class != TYPE_UNKNOWN);
    u32 size = type_sizes[type_class];
    assert(size <= TYPE_MAX_SIZE);
    return size;
}


Type * get_ret_type(Type function) {
    assert_eq(function.class, TYPE_FUNCTION);
    return &((FunctionTypeData *) function.data)->ret_type;
}


Type * get_param_types(Type function) {
    assert_eq(function.class, TYPE_FUNCTION);
    return (Type *) ((void *) function.data + sizeof(FunctionTypeData));
}


u32 get_param_count(Type function) {
    assert_eq(function.class, TYPE_FUNCTION);
    return ((FunctionTypeData *) function.data)->param_count;
}


bool is_type_incomplete(Type type) {
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


TypeClass str_to_type_class(slice_t str) {
    for (u32 i = 0; i < arrlen(type_names); i++) {
        if (!type_names[i]) continue;
        if (slice_str_eq(str, type_names[i])) return i;
    }
    return TYPE_NONE;
}


void fmt_type(dslice_t * buf, Type type) {
    if (type.class == TYPE_UNKNOWN)
        da_append_str(*buf, "<unknown type>");
    else if (type.class == TYPE_NONE)
        da_append_str(*buf, "<not a type>");
    else if (is_type_primitive(type.class))
        da_append_str(*buf, type_names[type.class]);
    else if (type.class == TYPE_FUNCTION) {
        da_append(*buf, '(');
        u32 param_count = get_param_count(type);

        for (u32 i = 0; i < param_count; i++) {
            fmt_type(buf, get_param_types(type)[i]);
            da_append(*buf, ' ');
        }
        da_append_str(*buf, "-> ");
        fmt_type(buf, *get_ret_type(type));
        da_append(*buf, ')');
    }    
}


u32 match_type_sh(Type type, const char * sh) {
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


bool match_param_types_sh(const char * sh, const Type * types, u32 count) {
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
