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
count = 0

foo = func ():
  capture count
  count += 1
  coyield
  count += 1

fiber = coinit foo()
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

fiber = coinit foo()
coresume fiber
```
`coresume` also returns the resulting value. In a future version of Cyber, you will be able to yield back results and pass values back when resuming.
```cy
func foo():
  return 123

fiber = coinit foo()
print(coresume fiber)    -- '123'
```
Use `Fiber.status()` to get the current state of the fiber.
```cy
func foo():
  coyield
  print 'done'

fiber = coinit foo()
print fiber.status()   -- '#paused'
coresume fiber
print fiber.status()   -- '#paused'
coresume fiber
print fiber.status()   -- '#done'
```
The main execution context is a fiber as well. Once the main fiber has finished, the VM is done and control is returned to the host.

## Gas mileage.

## Async.

## Multi-thread.