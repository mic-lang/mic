# The Mic Programming Language

## Installation
Install gcc, opam(>=2.1) and OCaml(>=4.13) in your enviroment, then run the following command to install dependencies.
```bash
$ opam install dune menhir ppx_driving
```

## To compile a *.mi to *.o
```bash
$ dune exec bin/main.exe mi_test.mi
```

## To execute the binary output
```bash
$ ./mi_test.o
```