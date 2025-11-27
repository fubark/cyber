#[bind] var a

--cytest: error
--ParseError: Expected declaration statement.
--
--@MainPath():1:9:
--#[bind] var a
--        ^
--