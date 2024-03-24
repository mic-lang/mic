# The Mic Programming Language
Usage

## To transpile a *.mi to a *.c
```bash
$ dune exec bin/main.exe example.mi
```

## To compile a generated *.c file
```bash
$ gcc example.c -lmimalloc
```