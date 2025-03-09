#include <mimalloc.h>

#define mi(i)  __attribute__((__address_space__(i)))

typedef const __region_t region;

constexpr region _static = 0;

lifetime <region p>
mi_heap_t mi(p)* mic_heap_new(void) {
    return (mi_heap_t mi(p)*)mi_heap_new();
}

lifetime <region p>
static inline void       mic_heap_delete(mi_heap_t mi(p)* heap) {
    mi_heap_delete((mi_heap_t*)heap);
    return;
}

lifetime <region p>
static  void       mic_heap_destroy(mi_heap_t mi(p)* heap) {
    mi_heap_destroy((mi_heap_t*)heap);
    return;
}

lifetime <region p>
static inline void mi(p)* mic_heap_malloc(mi_heap_t mi(p)* heap, size_t size) {
    return (mi_heap_t mi(p)*)mi_heap_malloc((mi_heap_t*)heap, size);
}

lifetime <region p>
static inline void mi(p)* mic_heap_zalloc(mi_heap_t mi(p)* heap, size_t size) {
    return (mi_heap_t mi(p)*)mi_heap_zalloc((mi_heap_t*)heap, size);
}

lifetime <region p>
static inline void mi(p)* mic_heap_calloc(mi_heap_t mi(p)* heap, size_t count, size_t size) {
    return (mi_heap_t mi(p)*)mi_heap_calloc((mi_heap_t*)heap, count, size);
}

lifetime <region p>
static inline void mi(p)* mic_heap_mallocn(mi_heap_t mi(p)* heap, size_t count, size_t size) {
    return (mi_heap_t mi(p)*)mi_heap_mallocn((mi_heap_t*)heap, count, size);
}

lifetime <region p>
static inline void mi(p)* mic_heap_malloc_small(mi_heap_t mi(p)* heap, size_t size) {
    return (mi_heap_t mi(p)*)mi_heap_malloc_small((mi_heap_t*)heap, size);
}

lifetime <region p>
static inline void mi(p)* mic_heap_realloc(mi_heap_t mi(p)* heap, void mi(p)* ptr, size_t newsize) {
    return (mi_heap_t mi(p)*)mi_heap_realloc((mi_heap_t*)heap, (void*)ptr, newsize);
}

lifetime <region p>
static inline void mi(p)* mic_heap_reallocn(mi_heap_t mi(p)* heap, void mi(p)* ptr, size_t count, size_t size) {
    return (mi_heap_t mi(p)*)mi_heap_reallocn((mi_heap_t*)heap, (void*)ptr, count, size);
}

lifetime <region p>
static inline void mi(p)* mic_heap_reallocf(mi_heap_t mi(p)* heap, void mi(p)* ptr, size_t newsize) {
    return (mi_heap_t mi(p)*)mi_heap_reallocf((mi_heap_t*)heap, (void*)ptr, newsize);
}

lifetime <region p>
static inline void mic_free(void mi(p)* ptr) {
    mi_free((void*)ptr);
    return;
}
