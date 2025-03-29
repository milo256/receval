#pragma once
#include "common.h"


enum VarLocation { GLOBAL, LOCAL };

Ident ident_new(u32 loc, u32 ofs);

u32 var_location(Ident ident);

u32 var_offset(Ident ident);
