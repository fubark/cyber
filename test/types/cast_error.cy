var a = 123
print(a as float)

--cytest: error
--CompileError: Cast expects `float`, got `int`.
--
--main:2:12:
--print(a as float)
--           ^
--