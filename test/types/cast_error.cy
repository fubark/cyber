var a = 123
print(a as pointer)

--cytest: error
--CompileError: Cast expects `pointer`, got `int`.
--
--main:2:9:
--print(a as pointer)
--        ^
--