#pragma once
#include "common.h"


void parse_code(char * code, void ** out_globals, Function ** out_main_fn);

void free_code(void);
