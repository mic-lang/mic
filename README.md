# The Mic clang repo

This repo contains a patch of the LLVM/Clang toolchain that has been modified to support  Mic. 
Mic extends C with region based memory management using mimmaloc.
The Mic documentation is available at the
[The Mic book](https://mic-lang.github.io/).

## Trying out Mic

You can install the Mic compiler with the foolowing steps.

```
git clone https://github.com/llvm/llvm-project 
cd llvm-project
git checkout 3026ecaff54b220409ecc254b4f6209801a251b9
git apply mic.patch
mkdir build
cd build
cmake -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release -G "Unix Makefiles" ../llvm
make -j4
```

Try it out

```
llvm-project/build/bin/clang -std=mic main.c
```