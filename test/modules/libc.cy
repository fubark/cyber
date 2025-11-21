use lc 'src/std/libc.cy'
use test
use meta

fn checkTypes():
    #if meta.system() == .macos:
        test.eq(144, type.size(lc.macos.Stat))

checkTypes()

--cytest: pass