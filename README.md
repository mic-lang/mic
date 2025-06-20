# The Mic patch repo

This repo contains a patch of the LLVM/Clang toolchain that has been modified to support  Mic. 
Mic extends C with region based memory management using mimalloc.
The Mic documentation is available at the
[The Mic book](https://mic-lang.github.io/).

## Trying out Mic

The binaries are available at 
https://github.com/mic-lang/clang/releases/tag/mic-19.1.7

You can build the Mic llvm/clang compiler with the following steps.

```
git clone https://github.com/llvm/llvm-project 
cd llvm-project
git checkout llvmorg-19.1.7
git apply --directory=clang llvm-19.1.7.patch
mkdir build
cd build
cmake -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release -G "Unix Makefiles" ../llvm
make -j4
```

Try it out

```
bin/clang -std=mic main.c /usr/local/lib/libmimalloc.so
```