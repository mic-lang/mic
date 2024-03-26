#include <mimalloc.h>

#include <stdbool.h>

#include <stddef.h>

typedef long unsigned int size_t;
typedef long int ptrdiff_t;
size_t strlen(const char *);
int strcmp(const char *, const char *);
char *strchr(const char *, int);
char *strcpy(char *, const char *);
void *memset(void *, int, size_t);
void *memcpy(void *, const void *, size_t);
int strncmp(const char *, const char *, size_t);
char *strncpy(char *, const char *, size_t);

size_t inline static mi_strlen(const char *s) {
    {
        return strlen(s);
    }
}

inline static int mi_strcmp(const char *l, const char *r) {
    {
        return strcmp(l, r);
    }
}

size_t inline static mi_strchr(const char *s, int) {
    {
        return strchr(s);
    }
}

inline static int mi_strcpy(char *dst, const char *src) {
    {
        return strcpy(dst, src);
    }
}

inline static void mi_memset(void *buf, int n, size_t size) {
    {
        memset(buf, n, size);
return;
    }
}

inline static void mi_memcpy(char *dst, void *src, size_t size) {
    {
        memcpy(dst, src, size);
return;
    }
}

inline static int mi_strncmp(const char *l, const char *r, size_t size) {
    {
        return strncmp(l, r, size);
    }
}

inline static int mi_strncpy(char *dst, const char *src, size_t size) {
    {
        return strncpy(dst, src, size);
    }
}
