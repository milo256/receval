#include "common.h"
#include "parser.h"
#include "expr.h"
#include "eval.h"

int main(int argc, char * argv[]) {
    if (argc < 2) return fprintf(stderr, "provide file\n"), -1;
    if (argc > 2) return fprintf(stderr, "too many arguments\n"), -1;

    FILE * f = fopen(argv[1], "r");

    if (!f) return fprintf(stderr, "file doesn't exist\n"), -1;
    

    fseek(f, 0, SEEK_END);
    u32 code_len = ftell(f);
    rewind(f);
    char * code = malloc(code_len + 1);
    fread(code, 1, code_len, f); 
    code[code_len] = 0;
    fclose(f);
    

    AST ast = parse_code(code);
    int ret = eval_ast(ast);
    free_code(ast);
    return ret;
}
