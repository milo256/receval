#pragma once

#include "common.h"

typedef struct {
    void * mem, * fill_ptr;
} Arena;

Arena arena_init(void);
void * aalloc(Arena *, u32);
void afree(Arena);
