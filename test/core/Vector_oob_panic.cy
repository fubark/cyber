arr := [3]int{1, 2, 3}
res := arr[15]

--cytest: panic
--panic: Out of bounds index: 15
--
--[trace]
--main:2:8 main:
--res := arr[15]
--       ^
--