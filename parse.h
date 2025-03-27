#pragma once

#include "common.h"

Ident ident_new(u32 loc, u32 ofs);
u32 var_location(Ident ident);
u32 var_offset(Ident ident);

void parse_code(char * code, void ** out_globals, Function ** out_main_fn);
