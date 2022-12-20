local count = 0

function inc()
    count = count + 1
    coroutine.yield()
    count = count + 1
end

local list = {}
for i = 0, 100000-1 do
    f = coroutine.create(inc)
    coroutine.resume(f)
    list[i] = f
end

for k, f in pairs(list) do
    coroutine.resume(f)
end

io.write(count .. "\n")
