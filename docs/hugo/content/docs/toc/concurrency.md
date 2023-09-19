---
title: "Concurrency"
weight: 8
---

# Concurrency.
Cyber supports fibers as a concurrency mechanism. There are plans to support preemptive concurrency with async/await as well as multithreading.

## Fibers.
Fibers in Cyber allow representing execution contexts as first-class values. They contain their own call stack and program counters. Fibers by themselves do not enable parallelism.

The `coinit` creates a new fiber from a function call syntax. Using `coyield` inside a function pauses the current fiber and execution is returned to the fiber that invoked `coresume`.
```cy
var count = 0

var foo = func ():
  count += 1
  coyield
  count += 1

var fiber = coinit foo()
print count          -- '0'
coresume fiber
print count          -- '1'
coresume fiber
print count          -- '2'
```
In Cyber, `coyield` can be used anywhere in a fiber's call stack.
```cy
func foo():
  print 'foo'
  bar()

func bar():
  -- Nested coyield in call stack.
  coyield
  print 'bar'

var fiber = coinit foo()
coresume fiber
```
`coresume` also returns the resulting value.
```cy
func foo():
  return 123

var fiber = coinit foo()
print(coresume fiber)    -- '123'
```

`coyield` can return a value back to `coresume`.
> _Planned Feature_

Use `Fiber.status()` to get the current state of the fiber.
```cy
func foo():
  coyield
  print 'done'

var fiber = coinit foo()
print fiber.status()   -- '#paused'
coresume fiber
print fiber.status()   -- '#paused'
coresume fiber
print fiber.status()   -- '#done'
```
The main execution context is a fiber as well. Once the main fiber has finished, the VM is done and control is returned to the host.

## Gas mileage.
> _Planned Feature_

## Async.
> _Planned Feature_

## Multi-thread.
> _Planned Feature_
