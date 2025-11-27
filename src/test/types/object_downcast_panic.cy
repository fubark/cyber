a := Object(^123)
print(a.downcast(^float).*)

--cytest: panic
--panic: Downcast expected `^float` typeid=13, found typeid=6.
--
--[trace]
--@MainPath():2:7 main:
--print(a.downcast(^float).*)
--      ^
--