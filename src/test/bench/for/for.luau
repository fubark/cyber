local start = os.clock()
local list = {}
for i = 0, 999999 do
  list[i] = i
end

local sum = 0
for k, i in pairs(list) do
  sum = sum + i
end

print("time: ", (os.clock() - start) * 1000, "\n")
print(sum .. "\n")