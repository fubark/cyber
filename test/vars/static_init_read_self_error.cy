var .c = c

--cytest: error
--CompileError: Reference to `c` creates a circular dependency.
--
--main:1:10:
--var .c = c
--         ^
--