a := 10
b := &a

lift := ^b

--cytest: error
--CompileError: Cannot lift the borrow type `&int`.
--
--@MainPath():4:9:
--lift := ^b
--        ^~
--