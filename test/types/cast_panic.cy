dyn a = 123
print(a as float)

--cytest: error
--panic: Can not cast `int` to `float`.
--
--main:2:7 main:
--print(a as float)
--      ^
--