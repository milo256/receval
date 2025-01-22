#pragma once

#define MAX(A,B) ((A > B) ? A : B)
#define MIN(A,B) ((A > B) ? B : A)

#define ASSERT(A) do{ if (!(A)) {                                                      \
    fprintf(stderr, "Assertion failed on %s:%d: %s = %d\n", __FILE__, __LINE__, #A, A);\
    exit(1);                                                                           \
}} while(0)

#define PANIC(...) do {                                  \
    fprintf(stderr, "Panic! %s:%d\n", __FILE__, __LINE__); \
    exit(-1);                                            \
} while(0)

#define ARRLEN(A) (sof(A)/sof(A[0]))
