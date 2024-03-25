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
int fputs(char *, FILE *);
char *fgets(char *, int, FILE *);
char *gets(char *);


int printf(const char *, ...);
int fprintf(FILE *, const char *, ...);
int sprintf(char *, const char *, ...);


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
inline static char p a* mi_fgets(char p a*drop s, int n, int FILE* f) {
    unsafe {
        return fgets(s, n, f);
    }
}

lifetime <depth p, kind a>
inline static char p a* mi_gets(char p a s) {
    unsafe {
        return gets(s);
    }
}

#endif  /* stdio.h */