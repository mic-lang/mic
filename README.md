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

## Documantation
Read [The Mic Book](https://mic-lang.github.io/).

## Special Thanks.
To implement this mic implemantation, I used following resources as the reference.
 https://github.com/jhjourdan/C11parser.

 https://github.com/keiichiw/ucc
 
 https://github.com/rui314/chibicc

 Thank you so much :)