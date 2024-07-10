-- Narrow cast defers to runtime cast.
var a any = 123
print(a as float)

--cytest: error
--panic: Can not cast `int` to `float`.
--
--main:3:7 main:
--print(a as float)
--      ^
--