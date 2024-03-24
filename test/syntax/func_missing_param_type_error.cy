func foo(a):
    pass

--cytest: error
--ParseError: Expected param type.
--
--main:1:11:
--func foo(a):
--          ^
--