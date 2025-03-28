#include "tokenizer.h"

#include "da.h"

#include <ctype.h>
#include <string.h>


static u32 long_token_class(LStr str) {
    if (isdigit(str.chars[0]))
        return TK_INT;
    if (lstr_eq(str, LSTR("function")))
        return TK_FUNCTION;
    return TK_IDENT;
}


static u32 char_token_class(char ch) {
    switch(ch) {
        case '=': return TK_ASSIGN;
        case '(': return TK_OPEN;
        case ')': return TK_CLOSE;
        case '{': return TK_CB_OPEN;
        case '}': return TK_CB_CLOSE;
        case '[': return TK_SB_OPEN;
        case ']': return TK_SB_CLOSE;
        default: return TK_NONE;
    }
}


Token * tokenize(char * code, Arena * arena) {
    #define make_token(class) (Token) { class, token_str, line, ts - line_start }

    da(Token) token_list = {};

    u32 line = 1, line_start = 0, ts = 0, tlen = 0;
    char ch;

    LStr token_str;

    while ((ch = code[ts + tlen])) {
        u32 char_token = char_token_class(ch);
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
            da_append(token_list, make_token(long_token_class(token_str)));
            ts += tlen;
            tlen = 0;
        }
        if (char_token) {
            token_str = (LStr) { &code[ts], 1 };
            da_append(token_list, make_token(char_token));
            ts++;
        }
        if (arrow) {
            token_str = (LStr) { &code[ts], 2 };
            da_append(token_list, make_token(TK_ARROW));
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
        if (!code[ts + tlen - 1]) break;
    }

    token_str = (LStr) { &code[ts - 1], 1 };
    da_append(token_list, make_token(TK_EOF));


    u32 n = token_list.len * sizeof(Token);
    Token * tokens = aalloc(arena, n);
    memcpy(tokens, token_list.items, n);

    free(token_list.items);

    return tokens;
    #undef make_token
}


void print_tokens(const Token * tokens) {
    while (tokens->class != TK_EOF) {
        char * buf = malloc(tokens->val.len + 1);
        buf[tokens->val.len] = 0;
        strncpy(buf, tokens->val.chars, tokens->val.len);
        printf("%d(%s)\n", tokens->class, buf);
        free(buf);
        tokens++;
    }
}

