type S:
    a Object

fn foo() -> int:
    return 123

s := ^S{a=Object(^foo())}
s = ^S{a=Object(^123)}

--cytest: pass