type Foo[T type]:
    a T

fn Foo[] :: bar(self):
    pass

Foo.bar()

--cytest: error
--CompileError: `bar` was declared as a template instance function. Expand the template first.
--
--@MainPath():7:5:
--Foo.bar()
--    ^~~
--