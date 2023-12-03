import time

start = time.process_time()
count = 0

def inc():
    global count
    count += 1
    yield
    count += 1
    yield

list = []
for i in range(0, 100000):
    f = inc()
    next(f)
    list.append(f)

for f in list:
    next(f)

print("time: " + str((time.process_time() - start)*1000))
print(count)
