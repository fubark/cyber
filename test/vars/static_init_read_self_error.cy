var .c = c

--cytest: error
--CompileError: Referencing `c` creates a circular dependency.
--
--main:1:10:
--var .c = c
--         ^
--