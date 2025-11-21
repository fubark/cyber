fn main():
    pass

fn main(a int):
    pass

--cytest: error
--CompileError: Expected only one `main` function.
--
--main:1:1:
--fn main():
--^~~~~~~~~~
--