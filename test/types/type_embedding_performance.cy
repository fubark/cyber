use t 'test'

-- Performance tests for type embedding
-- These tests measure the efficiency of embedded field access

type PerfBase:
    x int
    y int
    z int
    w int

type PerfContainer:
    base use PerfBase
    direct int

-- Test that embedded field access has no performance penalty
var perf = PerfContainer{
    base=PerfBase{x=1, y=2, z=3, w=4},
    direct=5
}

-- Direct field access (baseline)
t.eq(perf.direct, 5)

-- Embedded field access (should be equally fast)
t.eq(perf.base.x, 1)
t.eq(perf.base.y, 2)
t.eq(perf.base.z, 3)
t.eq(perf.base.w, 4)

-- Test field assignment performance
perf.base.x = 10
perf.base.y = 20
perf.base.z = 30
perf.base.w = 40
t.eq(perf.base.x, 10)
t.eq(perf.base.y, 20)
t.eq(perf.base.z, 30)
t.eq(perf.base.w, 40)

-- Test method access performance
type MethodPerfBase:
    value int
    
    func getValue(self) int:
        return self.value
    
    func setValue(self, v int):
        self.value = v

type MethodPerfContainer:
    base use MethodPerfBase
    extra int

var meth_perf = MethodPerfContainer{
    base=MethodPerfBase{value=100},
    extra=200
}

-- Method calls through embedding should be efficient
t.eq(meth_perf.base.getValue(), 100)
meth_perf.base.setValue(300)
t.eq(meth_perf.base.getValue(), 300)

-- Test multiple level access performance
type MultiLevelBase:
    level1 int

type MultiLevelMid:
    base use MultiLevelBase
    level2 int

type MultiLevelTop:
    mid use MultiLevelMid
    level3 int

var multi = MultiLevelTop{
    mid=MultiLevelMid{
        base=MultiLevelBase{level1=1},
        level2=2
    },
    level3=3
}

t.eq(multi.mid.base.level1, 1)
t.eq(multi.mid.level2, 2)
t.eq(multi.level3, 3)

-- Stress test: many embedded fields
type StressBase1:
    a1 int
type StressBase2:
    a2 int
type StressBase3:
    a3 int
type StressBase4:
    a4 int
type StressBase5:
    a5 int

type StressContainer:
    b1 use StressBase1
    b2 use StressBase2
    b3 use StressBase3
    b4 use StressBase4
    b5 use StressBase5
    direct int

var stress = StressContainer{
    b1=StressBase1{a1=1},
    b2=StressBase2{a2=2},
    b3=StressBase3{a3=3},
    b4=StressBase4{a4=4},
    b5=StressBase5{a5=5},
    direct=0
}

t.eq(stress.b1.a1, 1)
t.eq(stress.b2.a2, 2)
t.eq(stress.b3.a3, 3)
t.eq(stress.b4.a4, 4)
t.eq(stress.b5.a5, 5)
t.eq(stress.direct, 0)

-- Test that object creation with embeddings is efficient
var fast_create = PerfContainer{
    base=PerfBase{x=100, y=200, z=300, w=400},
    direct=500
}
t.eq(fast_create.base.x, 100)
t.eq(fast_create.direct, 500)

--cytest: pass
