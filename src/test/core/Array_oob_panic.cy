a := Array[int]{1, 2, 3}
_ = a[100]

--cytest: panic
--panic: Out of bounds index: 100
--
--[trace]
--@MainPath():2:5 main:
--_ = a[100]
--    ^
--