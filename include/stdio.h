#ifndef _MIC_STDIO_H
#define _MIC_STDIO_H

#include "stdarg.h"
#include "stddef.h"

#define EOF (-1)

typedef struct _IO_FILE FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

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

lifetime <depth p, kind a>
inline static int mi_puts(char p a* s) {
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
        int done;
        va_list ap;
        va_start(ap, fmt);
        done = vfprintf(stdout, fmt, ap);
        va_end(ap);
        return done;
    }
}

lifetime <depth p, kind a>
inline static int mi_fprintf(FILE* fp, const char p a* fmt, ...) {
    unsafe {
        int done;
        va_list ap;
        va_start(ap, fmt);
        done = vfprintf(fp, fmt, ap);
        va_end(ap);
        return done;
    }
}

lifetime <depth p, depth q, kind a, kind b>
inline static int mi_sprintf(char p a* s, const char q b* fmt, ...) {
    unsafe {
        int done;
        va_list ap;
        va_start(ap, fmt);
        done = vsprintf(s, fmt, ap);
        va_end(ap);
        return done;
    }
}

#endif  /* stdio.h */