#ifndef _MIC_STRING_H
#define _MIC_STRING_H

#include "stddef.h"

size_t strlen(const char *);
int strcmp(const char *, const char *);
char *strchr(const char *, int);
char *strcpy(char *, const char *);
void *memset(void *, int, size_t);
void *memcpy(void *, const void *, size_t);
int strncmp(const char *, const char *, size_t);
char *strncpy(char *, const char *, size_t);

lifetime <depth p, kind a>
size_t inline static mi_strlen(const char p a* s) {
    unsafe {
        return strlen(s);
    }
}

lifetime <depth p, depth q, kind a, kind b>
inline static int mi_strcmp(const char p a* l, const char q b* r) {
    unsafe {
        return strcmp(l, r);
    }
}

lifetime <depth p, kind a>
size_t inline static mi_strchr(const char p a* drop s, int) {
    unsafe {
        return strchr(s);
    }
}

lifetime <depth p, depth q, kind a, kind b>
inline static int mi_strcpy(char p a* dst, const char q b* src) {
    unsafe {
        return strcpy(dst, src);
    }
}

lifetime <depth p, depth q, kind a, kind b>
inline static void mi_memset(void p a* buf, int n, size_t size) {
    unsafe {
        memset(buf, n, size);
        return;
    }
}

lifetime <depth p, depth q, kind a, kind b>
inline static void mi_memcpy(char p a* dst, void q b* src, size_t size) {
    unsafe {
        memcpy(dst, src, size);
        return;
    }
}

lifetime <depth p, depth q, kind a, kind b>
inline static int mi_strncmp(const char p a* l, const char q b* r, size_t size) {
    unsafe {
        return strncmp(l, r, size);
    }
}

lifetime <depth p, depth q, kind a, kind b>
inline static int mi_strncpy(char p a* dst, const char q b* src, size_t size) {
    unsafe {
        return strncpy(dst, src, size);
    }
}

#endif  /* string.h */