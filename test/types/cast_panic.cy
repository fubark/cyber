use test
dyn a = test.erase(123)
print(a as float)

--cytest: error
--panic: Can not cast `int` to `float`.
--
--main:3:7 main:
--print(a as float)
--      ^
--