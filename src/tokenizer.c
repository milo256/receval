#include "tokenizer.h"

#include <ctype.h>
#include <string.h>



static u32 long_token_class(slice_t str) {
    if (isdigit(str.sptr[0]))
        return TK_INT;
    if (slice_eq(str, slice("function")))
        return TK_FUNCTION;
    return TK_IDENT;
}


static u32 symbolic_token_class(slice_t str) {
    if (slicelen(str) == 2 && str.sptr[0] == '-' && str.sptr[1] == '>')
        return TK_ARROW;
    else if (slicelen(str) == 1) switch(str.sptr[0]) {
        case '=': return TK_ASSIGN;
        case '(': return TK_OPEN;
        case ')': return TK_CLOSE;
        case '{': return TK_CB_OPEN;
        case '}': return TK_CB_CLOSE;
        case '[': return TK_SB_OPEN;
        case ']': return TK_SB_CLOSE;
    }
    return TK_NONE;
}

enum {
    CMT_NONE,
    CMT_LINE,
    CMT_BEGIN,
    CMT_END,
};

static u32 comment_class(const char * ch) {
    if (ch[0] == '-' && ch[1] == '-') return CMT_LINE;
    else if (ch[0] == '-' && ch[1] == '!') return CMT_BEGIN;
    else if (ch[0] == '!' && ch[1] == '-') return CMT_END;
    else return CMT_NONE;
}

static bool is_line_end(char ch) { return (!ch || ch == '\n' || ch == '\r'); }

static bool is_whitespace(char ch) {
    return (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r');
}

static bool is_splitting(const char * ch) {
    return
        is_whitespace(*ch) || (!*ch) || (*ch == '"') || comment_class(ch) ||
        symbolic_token_class((slice_t) {(char *) ch, (char *) ch + 1}) ||
        symbolic_token_class((slice_t) {(char *) ch, (char *) ch + 2});
}


enum {
    PARSE_EOF,
    PARSE_BLANK,
    PARSE_NORMAL,
    PARSE_CMT_LINE,
    PARSE_CMT_MULTILINE,
    PARSE_STRING,
};


typedef struct {
    u32 parsing;
    const char * sptr, * eptr;
    u32 line, col;
} State;


static slice_t curstr(const State * s) {
    return (slice_t) { .sptr = (char *) s->sptr, .eptr = (char *) s->eptr };
}


static void move_ptr(State * s, u32 dist) {
    s->eptr += dist;
    for (u32 i = 0; i < dist; i++)
        if (is_line_end(*++s->sptr))
            s->col = 0, s->line++;
        else
            s->col++;
}


static Token make_token(State * s, u32 class) {
    Token token = {
        .class = class,
        .val = curstr(s),
        .dbug_line = s->line, .dbug_column = s->col
    };
    s->parsing = PARSE_BLANK;
    move_ptr(s, slicelen(token.val));
    s->eptr = s->sptr;
    return token;
}

static State state_init(const char * code) { return (State) { PARSE_BLANK, code, code, 1, 1 }; }


void print_token(Token token);

static Token get_token(State * s) {
    u32 class;

    for (;;) switch (s->parsing) {
        case PARSE_BLANK:
            while (s->eptr == s->sptr && is_whitespace(*s->sptr))
                move_ptr(s, 1);

            u32 cmt = comment_class(s->sptr);

            if (cmt == CMT_END) panic("tokenizer error");
            else if (cmt == CMT_LINE)  s->parsing = PARSE_CMT_LINE;
            else if (cmt == CMT_BEGIN) s->parsing = PARSE_CMT_MULTILINE;
            else if (*s->sptr == '"')  s->parsing = PARSE_STRING;
            else if (!*s->sptr)        s->parsing = PARSE_EOF;
            else                       s->parsing = PARSE_NORMAL;

            break;;
        case PARSE_CMT_LINE:
            for (const char * ch = s->sptr;; ch++)
                if (is_line_end(*ch)) {
                    s->sptr = s->eptr = ch;
                    s->col = 0, s->line++;
                    s->parsing = PARSE_BLANK;
                    break;;
                }
            break;
        case PARSE_CMT_MULTILINE:
            move_ptr(s, 2);
            for (;*s->sptr && comment_class(s->sptr - 2) != CMT_END; move_ptr(s, 1));
            s->parsing = PARSE_BLANK;
            break;

        case PARSE_NORMAL: 
            for(;;) {
                s->eptr++;

                if ((class = symbolic_token_class(curstr(s))))
                    return make_token(s, class);
                else if (is_splitting(s->eptr))
                    return make_token(s, long_token_class(curstr(s)));
            }
        case PARSE_STRING:
            move_ptr(s, 1);
            while (*++s->eptr != '"')
                if (!*s->eptr) panic("tokenizer error");
            Token tk = make_token(s, TK_STR);
            move_ptr(s, 1);
            return tk;
        case PARSE_EOF:
            return make_token(s, TK_EOF);
    };
}


void print_token(Token token) {
    u32 len = slicelen(token.val);
    char * buf = malloc(len + 1);
    buf[len] = 0;
    strncpy(buf, token.val.sptr, len);
    printf("%d(%s)\n", token.class, buf);
    free(buf);
}


void print_tokens(const Token * tokens) {
    while (tokens->class != TK_EOF) {
        u32 len = slicelen(tokens->val);
        char * buf = malloc(len + 1);
        buf[len] = 0;
        strncpy(buf, tokens->val.sptr, len);
        printf("%d(%s)%c", tokens->class, buf,
            (tokens->val.sptr[len] == '\n' || tokens[1].class == TK_EOF) ?'\n':' '
        );
        free(buf);
        tokens++;
    }
}


Token * tokenize(const char * code, Arena * arena) {
    State s = state_init(code);

    da(Token) token_list = {};

    Token token;
    do {
        token = get_token(&s);
        da_append(token_list, token);
    } while (token.class != TK_EOF);

    u32 n = token_list.len * sizeof(Token);
    Token * tokens = aalloc(arena, n);
    memcpy(tokens, token_list.items, n);

    da_dealloc(token_list);

    return tokens;
}
