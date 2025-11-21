a := 10
b := &a

lift := ^b

--cytest: error
--CompileError: Cannot lift the borrow type `&int`.
--
--main:4:9:
--lift := ^b
--        ^~
--