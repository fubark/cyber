arr := [3]int{1, 2, 3}
res := arr[-1]

--cytest: panic
--panic: Out of bounds index: -1
--
--[trace]
--@MainPath():2:8 main:
--res := arr[-1]
--       ^
--