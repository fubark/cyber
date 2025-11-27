type Foo:
    a ^int = ^34
    b int

--cytest: error
--CompileError: Expected child type. Cannot lift values during const evaluation.
--
--@MainPath():2:14:
--    a ^int = ^34
--             ^~~
--@MainPath():1:1:
--type Foo:
--^~~~~~~~~
--