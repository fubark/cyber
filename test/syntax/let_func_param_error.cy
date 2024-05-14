let foo(a int):
    pass

--cytest: error
--ParseError: Expected `,`.
--
--main:1:11:
--let foo(a int):
--          ^
--