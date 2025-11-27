fn main(a int):
    pass

--cytest: error
--CompileError: Expected no parameters for `main` function.
--
--@MainPath():1:1:
--fn main(a int):
--^~~~~~~~~~~~~~~
--