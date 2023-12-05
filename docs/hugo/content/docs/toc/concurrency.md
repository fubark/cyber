---
title: "Concurrency"
weight: 8
---

# Concurrency.
Cyber supports fibers as a concurrency mechanism. There are plans to support preemptive concurrency with async/await as well as multithreading.

## Fibers.
A fiber represents a separate execution context as a first-class value. It contains it's own call stack and program counter. Fibers by themselves do not enable parallelism.

### Creating fibers.
The `coinit` keyword creates and returns a new fiber using a function as the entry point:
```cy
var count = 0

var foo = func ():
    count += 1
    coyield
    count += 1

var fiber = coinit(foo)

print count          -- '0'
coresume fiber
print count          -- '1'
coresume fiber
print count          -- '2'
```
A fiber does not start execution until `coresume` is invoked on it.
`coyield` pauses the current fiber and execution is returned to the previous fiber that invoked `coresume`.

### Passing arguments.
Arguments after the callee are passed into the entry function:
```cy
var count = 0

var increment = func (inc):
    count += inc

var fiber = coinit(increment, 5)
coresume fiber
print count          -- '5'
```
When the fiber is created, the arguments are saved inside the fiber's stack. Once the first `coresume` is invoked, the entry function is invoked with the saved arguments.

### Reset state.
To reset a fiber to its initial state, invoke `reset()`. {{<todo "*Planned Feature">}}
When reset, the existing stack is unwinded, the program counter returns to the starting point, and the state is set to `.init`:
```cy
func fib(n int) int:
    coyield n
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

var task = coinit(fib, 10)

-- Progress the fiber...
print(coresume task)    -- Prints "10"
print(coresume task)    -- Prints "9"
print(coresume task)    -- Prints "8"

-- Reset back to the start with the `.init` state.
fiber.reset()
print(coresume task)    -- Prints "10"
```

### Rebinding arguments.
Arguments attached to the fiber can be rebinded with a different set of values. {{<todo "*Planned Feature">}}
This allows fiber reuse, instead of creating a new fiber:
```cy
var task = coinit(fib, 10)

-- Run task to completion.
var res = 0
while fiber.status() != .done:
    res = coresume fiber
print res

fiber.reset()
fiber.bindArgs(20)

-- Run task again with the new argument...
```

### Fiber block.
A fiber block is used to construct a fiber without an entry function. {{<todo "*Planned Feature">}} The counting example can be rewritten to:
```cy
var count = 0

var fiber = coinit:
    count += 1       -- `count is captured`
    coyield
    count += 1

print count          -- '0'
coresume fiber
print count          -- '1'
coresume fiber
print count          -- '2'
```
Referencing parent variables from the fiber block automatically captures them just like a function closure.

### Pause and resume.
`coyield` can be used anywhere in a fiber's call stack to pause execution and return to the previous fiber.
```cy
func foo():
    print 'foo'
    bar()

func bar():
    -- Nested coyield in call stack.
    coyield
    print 'bar'

var fiber = coinit(foo)
coresume fiber
```
`coresume` also returns the resulting value.
```cy
func foo():
    return 123

var fiber = coinit(foo)
print(coresume fiber)    -- '123'
```

`coyield` can return a value back to `coresume`. {{<todo "*Planned Feature">}}

### Fiber state.
Use `Fiber.status()` to get the current state of the fiber.
```cy
func foo():
    coyield
    print 'done'

var fiber = coinit(foo)
print fiber.status()   -- '.paused'
coresume fiber
print fiber.status()   -- '.paused'
coresume fiber
print fiber.status()   -- '.done'
```
The main execution context is a fiber as well. Once the main fiber has finished, the VM is done and control is returned to the host.

## Gas mileage.
> _Planned Feature_

## Async.
> _Planned Feature_

## Multi-thread.
> _Planned Feature_
