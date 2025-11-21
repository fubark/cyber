type Foo:
    a ^int = ^34
    b int

--cytest: error
--CompileError: Expected child type. Cannot lift values during const evaluation.
--
--main:2:14:
--    a ^int = ^34
--             ^~~
--main:1:1:
--type Foo:
--^~~~~~~~~
--