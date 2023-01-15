func go():
    print 'start'
    coyield
    for 0..100 each i:
        print '{i}'
        coyield

f = coinit go()
for f.status() != #done:
    coresume f