#include "common.h"

typedef enum {
/* Tokenizer Errors
 * -----------------------------------------------------------------------------
 */
    NO_COMMENT_CLOSE,
    NO_COMMENT_OPEN,
    NO_STRING_CLOSE,
    BAD_CHARACTER,

/* Parser Errors
 * -----------------------------------------------------------------------------
 */
    /* Type Errors */
    BAD_PARAM_TYPE,
    BAD_PARAM_COUNT,
    BAD_BRANCH_TYPE,
    BAD_ASSIGN_TYPE,
    BAD_RETURN_TYPE,
    NOT_A_CONDITION,
    NOT_CALLABLE,

    /* Name Errors */
    UNRESOLVED_TYPE,
    UNDECLARED_IDENTIFIER,
    IDENTIFIER_REDECLARATION,
    BUILTIN_REDECLARATION,
    NOT_A_TYPE,

    /* Syntax Errors */
    NO_CLOSING_DELIM,
    NO_PARAM_LIST,
    NO_RETURN_TYPE,
    BAD_PARAM_NAME,
    BAD_EXPRESSION,
    NOT_AN_LVALUE
} ErrorClass;



struct {
    char * start;
    int len;
} Mark;

typedef struct {
    char * 
    ErrorClass class;
} Error;





struct {
    da(char) * lines;
} DiagMsg;
