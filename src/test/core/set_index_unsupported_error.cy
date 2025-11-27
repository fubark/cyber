1[0] = 2

--cytest: error
--CompileError: Unsupported type `int` for index assignment.
--
--@MainPath():1:1:
--1[0] = 2
--^~~~
--