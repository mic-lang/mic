#include <mimalloc.h>

#include <stdbool.h>

#include <stddef.h>

typedef __builtin_va_list va_list;
typedef long unsigned int size_t;
typedef long int ptrdiff_t;
typedef struct _IO_FILE FILE;
FILE extern *stdin;
FILE extern *stdout;
FILE extern *stderr;
int putchar(int);
int getchar(void);
int putc(int, FILE *);
int fputc(int, FILE *);
int getc(FILE *);
int fgetc(FILE *);
int puts(char *);
int fputs(const char *, FILE *);
char *fgets(char *, int, FILE *);
char *gets(char *);
int printf(const char *, ...);
int fprintf(FILE *, const char *, ...);
int sprintf(char *, const char *, ...);
int vprintf(const char *, va_list);
int vfprintf(FILE *, const char *, va_list);
int vsprintf(char *, const char *, va_list);

inline static int mi_puts(char *s) {
    {
        return puts(s);
    }
}

inline static int mi_fputs(const char *s, FILE *f) {
    {
        return fputs(s, f);
    }
}

inline static char *mi_fgets(char *s, int n, FILE *f) {
    {
        return fgets(s, n, f);
    }
}

inline static char *mi_gets(char *s) {
    {
        return gets(s);
    }
}

inline static int mi_printf(const char *fmt, ...) {
    {
        va_list ap;
        __builtin_va_start (ap, fmt);
        return vfprintf(stdout, fmt, ap);
    }
}

inline static int mi_fprintf(FILE *fp, const char *fmt, ...) {
    {
        va_list ap;
        __builtin_va_start (ap, fmt);
        return vfprintf(fp, fmt, ap);
    }
}

inline static int mi_sprintf(char *s, const char *fmt, ...) {
    {
        va_list ap;
        __builtin_va_start (ap, fmt);
        return vsprintf(s, fmt, ap);
    }
}
void __assert_fail(const char *, const char *, int);
void abort(void);
int abs(int);
long strtol(const char *, char **, int);
int rand(void);
void srand(unsigned);
struct mi_heap_s;
typedef struct mi_heap_s mi_heap_t;
mi_heap_t *mi_heap_new(void);
void mi_heap_delete(mi_heap_t *);
void mi_heap_destroy(mi_heap_t *);
void *mi_heap_malloc(mi_heap_t* p, size_t size);
void *mi_heap_zalloc(mi_heap_t* p, size_t size);
void *mi_heap_calloc(mi_heap_t* p, size_t count, size_t size);
void *mi_heap_realloc(mi_heap_t* p, void *ptr, size_t newsize);
void mi_free(void *p);
void mi_collect(bool force);
void mi_stats_print(void *out);

void test_heap(void *p_out) {
    mi_heap_t* q = mi_heap_new();
    void *p1 = mi_heap_malloc(q, 32);
    void *p2 = mi_heap_malloc(q, 48);
    mi_free(p_out);
    mi_free(p1);
    mi_free(p2);
    mi_heap_destroy(q);
}

int main() {
    {
        mi_heap_t* p = mi_heap_new();
        test_heap(mi_heap_malloc(p, 32));
        mi_heap_destroy(p);
    }
    mi_collect(1);
    mi_printf("hello, world!\n");
    return 0;
}
