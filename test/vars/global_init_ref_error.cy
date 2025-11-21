global a int = 123
global b int = a

--cytest: error
--CompileError: Cannot reference globals in a global initializer.
--
--main:2:16:
--global b int = a
--               ^
--