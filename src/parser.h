#pragma once
#include "expr.h"


AST parse_code(const char * code);

void free_code(AST);
