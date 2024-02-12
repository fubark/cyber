type Foo:
    var val int

var a = [Foo val: 123]
a = [+Foo val: 123]

--cytest: error
--CompileError: Expected type `Foo`, got `+Foo`.
--
--main:5:5:
--a = [+Foo val: 123]
--    ^
--