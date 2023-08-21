func go():
    print 'start'
    coyield
    for 0..100 each i:
        print '{i}'
        coyield

var f = coinit go()
while f.status() != #done:
    coresume f