fn bar():
    throw error.boom

fn foo():
    bar()

foo()

--cytest: error
--panic: error.boom
--
--main:2:5 bar:
--    throw error.boom
--    ^
--main:5:5 foo:
--    bar()
--    ^
--main:7:1 main:
--foo()
--^
--