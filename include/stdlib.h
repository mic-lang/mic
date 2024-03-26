#ifndef _MIC_STDLIB_H
#define _MIC_STDLIB_H

#include "stddef.h"
#include "stdbool.h"

#define RAND_MAX 2147483647

void abort(void);
int abs(int);
long strtol(const char *, char **, int);
int rand(void);
void srand(unsigned);

struct mi_heap_s;
typedef struct mi_heap_s mi_heap_t;
mi_heap_t* mi_heap_new(void);
void       mi_heap_delete(mi_heap_t*);
void       mi_heap_destroy(mi_heap_t*);
lifetime <depth p>
void p dyn* mi_heap_malloc(depth p, size_t size);
lifetime <depth p>
void p dyn* mi_heap_zalloc(depth p, size_t size);
lifetime <depth p>
void p dyn* mi_heap_calloc(depth p, size_t count, size_t size);
lifetime <depth p>
void p dyn* mi_heap_realloc(depth p, void* ptr, size_t newsize);
lifetime <depth p>
void mi_free(void p dyn* drop p);

void mi_collect(bool force);
void mi_stats_print(void* out);

#endif  /* stdlib.h */