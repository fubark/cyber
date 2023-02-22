var count = 0

func inc():
    static count
    count += 1
    coyield
    count += 1

fibers = []
for 0..100000:
    f = coinit inc()
    coresume f
    fibers.append(f)

for fibers each f:
    coresume f

print(count)
