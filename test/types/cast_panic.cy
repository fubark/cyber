use test
let a = test.erase(123)
print(a as pointer)

--cytest: error
--panic: Can not cast `int` to `pointer`.
--
--main:3:7 main:
--print(a as pointer)
--      ^
--