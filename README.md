# Implementation for LLVM's example: Kaleidocope.

## Building Steps
- Configure LLVM path
``` cmake -DCMAKE_BUILD_TYPE=Debug  -DLLVM_DIR=/Your/Path/to/LLVM/lib/cmake/llvm . ```
- Build
``` make -j4```