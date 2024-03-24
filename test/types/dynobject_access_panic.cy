type Foo dynobject:
    a int

var f = Foo{a: 123}
f.b

--cytest: error
--panic: Missing field in object.
--
--main:5:3 main:
--f.b
--  ^
--