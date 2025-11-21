-- Create libfoo.so using clang:
-- clang -shared -o libfoo.so foo.c

-- Create libfoo.so using gcc:
-- gcc -c -fPIC foo.c -o foo.o
-- gcc -shared foo.o -o libfoo.so

use c

#if meta.is_vm_target():
    #c.bind_lib('./libfoo.dylib')

#[extern]
fn add(a, b int) -> int

fn main():
    print(add(123, 321))