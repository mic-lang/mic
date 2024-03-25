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
void       mi_heap_delete(mi_heap_t* heap);
void       mi_heap_destroy(mi_heap_t* heap);
void* mi_heap_malloc(mi_heap_t* heap, size_t size);
void* mi_heap_zalloc(mi_heap_t* heap, size_t size);
void* mi_heap_calloc(mi_heap_t* heap, size_t count, size_t size);
void* mi_heap_realloc(mi_heap_t* heap, void* p, size_t newsize);
void mi_free(void* p);

void mi_collect(bool force);
void mi_stats_print(void* out);

lifetime <depth p>
inline static void p dyn* mi_malloc(mi_heap_t p dyn* heap, size_t size) {
    unsafe {
        return mi_heap_malloc(heap, size);
    }
}

lifetime <depth p>
inline static void p dyn* mi_zalloc(mi_heap_t p dyn* heap, size_t size) {
    unsafe {
        return mi_heap_zalloc(heap, size);
    }
}

lifetime <depth p>
inline static void p dyn* mi_calloc(mi_heap_t p dyn* heap, size_t count, size_t size) {
    unsafe {
        return mi_heap_calloc(heap, count, size);
    }
}

lifetime <depth p>
inline static void p dyn* mi_realloc(mi_heap_t p dyn* heap, void p dyn* ptr, size_t new_size) {
    unsafe {
        return mi_heap_realloc(heap, ptr, new_size);
    }
}

lifetime <depth p>
inline static void mi_free(void p dyn* drop ptr) {
    unsafe {
        return mi_free(ptr);
    }
}

#endif  /* stdlib.h */