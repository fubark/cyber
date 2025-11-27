start = Time.now

def fib(n)
  if n < 2 then
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

res = fib(30)
puts "time: " + ((Time.now - start) * 1000).to_s
puts res
