import time

start = time.process_time()

def fib(n):
  if n < 2: return n
  return fib(n - 1) + fib(n - 2)

res = fib(30)
print("time: " + str((time.process_time() - start)*1000))
print(res)
