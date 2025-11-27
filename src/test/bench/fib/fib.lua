local start = os.clock()
local function fib(n)
    if n < 2 then return n end
    return fib(n - 2) + fib(n - 1)
end

local res = fib(30)
io.write("time: ", (os.clock() - start) * 1000, "\n")
io.write(res, "\n")
