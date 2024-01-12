var Root.c = c

--cytest: error
--CompileError: Reference to `c` creates a circular dependency.
--
--main:1:14:
--var Root.c = c
--             ^
--