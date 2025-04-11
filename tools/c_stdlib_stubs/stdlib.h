#pragma once

/* */
#include <stddef.h>

#ifdef NULL
#  undef NULL
#endif
#define NULL 0
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

#define srandom(seed)
long int random(void);

#define qsort(...)

void exit(int status);

inline void *malloc(size_t n) {}
inline void *calloc(size_t nmemb, size_t size) { return malloc(nmemb * size); }
inline void *realloc(void *ptr, size_t size) { return malloc(size); }
inline void free(void *p) {}
