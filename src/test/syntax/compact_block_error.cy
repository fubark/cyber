if true: foo = 123
    foo = 234

--cytest: error
--ParseError: Unexpected indentation.
--
--@MainPath():2:5:
--    foo = 234
--    ^
--