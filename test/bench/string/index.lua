local str = string.rep('abcdefghijklmnopqrstuvwxyz123456', 1000000) .. 'waldo'

local start = os.clock()
local idx
for _ = 0, 99 do
    idx = string.find(str, 'waldo', 1, true)
end

io.write("idx: " .. idx .. " ms: " .. (os.clock() - start) * 1000 .. "\n")