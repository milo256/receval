#pragma once
#include "common.h"
#include "arena.h"


typedef enum {
    TK_NONE,
    /* opening delimiters are odd, closing even.
     * corresponding delimiters 1 apart */
    TK_OPEN     = 1,
    TK_CLOSE    = 2, 
    TK_SB_OPEN  = 3,
    TK_SB_CLOSE = 4,
    TK_CB_OPEN  = 5,
    TK_CB_CLOSE = 6,
    TK_INT,
    TK_FLOAT,
    TK_STRING,
    TK_FUNCTION,
    TK_ARROW,
    TK_IDENT,
    TK_ASSIGN,
    TK_EOF
} TkClass;


typedef struct {
    TkClass class;
    LStr val;
    u32 dbug_line;
    u32 dbug_column;
} Token;


#define tk(class) (Token) { .class = class, .str = NULL, .strlen = 0 }

void print_tokens(const Token * tokens);

Token * tokenize(char *, Arena *);

