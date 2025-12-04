use libc
use test
use meta

fn checkTypes():
    #if meta.system() == .macos:
        test.eq(144, type.size(libc.macos.Stat))

checkTypes()

--cytest: pass
