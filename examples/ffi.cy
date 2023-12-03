-- Create libfoo.so using clang:
-- clang -shared -o libfoo.so foo.c

-- Create libfoo.so using gcc:
-- gcc -c -fPIC foo.c -o foo.o
-- gcc -shared foo.o -o libfoo.so

import os

var ffi = os.newFFI()
ffi.cfunc('add', [.int, .int], .int)
-- my lib = ffi.bindLib('./libfoo.so')
my lib = ffi.bindLib('./libfoo.dylib')
print lib.add(123, 321)
