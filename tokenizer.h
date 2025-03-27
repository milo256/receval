#pragma once

#include "common.h"
#include "arena.h"

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

Token * tokenize(char *, Arena *);

