count = 0

func inc():
    count += 1
    coyield
    count += 1

fibers = []
for 0..100000:
    f = coinit inc()
    coresume f
    fibers.add(f)

for fibers as f:
    coresume f

print(count)