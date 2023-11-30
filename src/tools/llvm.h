// cyber src/tools/cbindgen.cy -o src/tools/llvm.cy src/tools/llvm.h -I/opt/homebrew/Cellar/llvm/17.0.5/include -libpath /opt/homebrew/Cellar/llvm/17.0.5/lib/libLLVM.dylib -stripPrefix LLVM
// Not needed anymore: -I/opt/homebrew/Cellar/llvm/17.0.5/lib/clang/17/include

#include "llvm-c/Core.h"
#include "llvm-c/Object.h"