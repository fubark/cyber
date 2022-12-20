def fib(n)
  if n < 2 then
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

puts fib(30)
