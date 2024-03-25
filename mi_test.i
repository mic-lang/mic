# 0 "mi_test.c"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "mi_test.c"
# 1 "include/stdio.h" 1



# 1 "include/stdarg.h" 1






typedef char *va_list;
# 5 "include/stdio.h" 2
# 1 "include/stddef.h" 1




typedef long unsigned int size_t;
typedef long int ptrdiff_t;
# 6 "include/stdio.h" 2



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
int fputs(char *, FILE *);
char *fgets(char *, int, FILE *);
char *gets(char *);


int printf(const char *, ...);
int fprintf(FILE *, const char *, ...);
int sprintf(char *, const char *, ...);

int vprintf(const char *, va_list);
int vfprintf(FILE *, const char *, va_list);
int vsprintf(char *, const char *, va_list);

lifetime <depth p, kind a>
inline static int mi_puts(const char p a* s) {
    unsafe {
        return puts(s);
    }
}

lifetime <depth p, kind a>
inline static int mi_fputs(const char p a* s, FILE* f) {
    unsafe {
        return fputs(s, f);
    }
}

lifetime <depth p, kind a>
inline static char p a* mi_fgets(char p a*drop s, int n, FILE* f) {
    unsafe {
        return fgets(s, n, f);
    }
}

lifetime <depth p, kind a>
inline static char p a* mi_gets(char p a* s) {
    unsafe {
        return gets(s);
    }
}

lifetime <depth p, kind a>
inline static int mi_printf(const char p a* fmt, ...) {
    unsafe {
        va_list ap;
        (ap = &(fmt) + 1);
        return vfprintf(stdout, fmt, ap);
    }
}

lifetime <depth p, kind a>
inline static int mi_fprintf(FILE* fp, const char p a* fmt, ...) {
    unsafe {
        va_list ap;
        (ap = &(fmt) + 1);
        return vfprintf(fp, fmt, ap);
    }
}

lifetime <depth p, depth q, kind a, kind b>
inline static int mi_sprintf(const char p a* s, const char q b* fmt, ...) {
    unsafe {
        va_list ap;
        (ap = &(fmt) + 1);
        return vsprintf(s, fmt, ap);
    }
}
# 2 "mi_test.c" 2
# 1 "include/assert.h" 1
# 10 "include/assert.h"
void __assert_fail(const char *, const char *, int);
# 3 "mi_test.c" 2
# 1 "include/stdlib.h" 1




# 1 "include/stdbool.h" 1
# 6 "include/stdlib.h" 2



void abort(void);
int abs(int);
long strtol(const char *, char **, int);
int rand(void);
void srand(unsigned);

struct mi_heap_s;
typedef struct mi_heap_s mi_heap_t;
mi_heap_t* mi_heap_new(void);
void mi_heap_delete(mi_heap_t* heap);
void mi_heap_destroy(mi_heap_t* heap);
void* mi_heap_malloc(mi_heap_t* heap, size_t size);
void* mi_heap_zalloc(mi_heap_t* heap, size_t size);
void* mi_heap_calloc(mi_heap_t* heap, size_t count, size_t size);
void* mi_heap_realloc(mi_heap_t* heap, void* p, size_t newsize);
void mi_free(void* p);

void mi_collect(int force);
void mi_stats_print(void* out);

lifetime <depth p>
inline static void p dyn* mi_malloc(depth p, size_t size) {
    unsafe {

    }
}

lifetime <depth p>
inline static void p dyn* mi_zalloc(depth p, size_t size) {
    unsafe {
        return mi_heap_zalloc(p, size);
    }
}

lifetime <depth p>
inline static void p dyn* mi_calloc(depth p, size_t count, size_t size) {
    unsafe {
        return mi_heap_calloc(p, count, size);
    }
}

lifetime <depth p>
inline static void p dyn* mi_realloc(depth p, void p dyn* ptr, size_t new_size) {
    unsafe {
        return mi_heap_realloc(p, ptr, new_size);
    }
}

lifetime <depth p>
inline static void mi_free(void p dyn* drop ptr) {
    unsafe {
        return mi_free(ptr);
    }
}
# 4 "mi_test.c" 2
# 1 "include/stdbool.h" 1
# 5 "mi_test.c" 2


void test_heap(void dyn* p_out) using q {
  void q dyn* p1 = mi_malloc<q>(q,32);
  void q dyn* p2 = mi_malloc<q>(q,48);
  mi_free<p>(p_out);
  mi_free<q>(p1); mi_free<q>(p2);
}

int main() using p {

  test_heap(mi_malloc<p>(p, 32));



  return 0;
}
