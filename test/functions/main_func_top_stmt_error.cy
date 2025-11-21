a := 1

fn main():
    pass

--cytest: error
--CompileError: Body statement is not allowed when main function is declared.
--
--main:1:1:
--a := 1
--^~~~~~
--