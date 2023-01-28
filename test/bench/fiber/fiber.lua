local cocreate = coroutine.create
local coresume = coroutine.resume
local coyield = coroutine.yield
local count = 0

local function inc()
    count = count + 1
    coyield()
    count = count + 1
end

local list = {}
for i = 1, 100000 do
    local f = cocreate(inc)
    coresume(f)
    list[i] = f
end

for _, f in ipairs(list) do
    coresume(f)
end

io.write(count, "\n")
