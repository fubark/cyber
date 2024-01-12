if true: foo = 123
    foo = 234

--cytest: error
--ParseError: Unexpected indentation.
--
--main:2:5:
--    foo = 234
--    ^
--