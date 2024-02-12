template(T type)
type Foo:
    var a #T

var f = [Foo#(String, int) a: 'abc']

--cytest: error
--CompileError: Expected template signature `Foo(type) type`.
--
--main:5:10:
--var f = [Foo#(String, int) a: 'abc']
--         ^
--