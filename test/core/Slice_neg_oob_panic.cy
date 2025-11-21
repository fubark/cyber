a := []int{1, 2, 3}
_ = a[-1]

--cytest: panic
--panic: Out of bounds index: -1
--
--[trace]
--main:2:5 main:
--_ = a[-1]
--    ^
--