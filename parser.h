#pragma once
#include "common.h"


void parse_code(
    char * code, void ** out_globals,
    Function ** out_main, bool * out_ignore_ret
);

void free_code(void);
