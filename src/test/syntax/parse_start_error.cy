global
global a = 123
global b = 234

--cytest: error
--ParseError: Expected global name.
--
--@MainPath():1:7:
--global
--      ^
--