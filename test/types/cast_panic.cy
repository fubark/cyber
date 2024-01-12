my a = 123
print(a as pointer)

--cytest: error
--panic: Can not cast `int` to `pointer`.
--
--main:2:9 main:
--print(a as pointer)
--        ^
--