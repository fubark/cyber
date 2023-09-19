---
title: "Memory"
bookFlatSection: true
weight: 12
---

# Memory.
Cyber provides memory safety by default.

## ARC.
Cyber uses ARC or automatic reference counting to manage memory.
ARC is deterministic and has less overhead compared to a tracing garbage collector. Reference counting distributes memory management, which reduces GC pauses and makes ARC suitable for realtime applications. One common issue in ARC implementations is reference cycles which Cyber addresses with [Weak References](#weak-references) and it's very own [Cycle Detection](#cycle-detection).

### Reference Counting.
In Cyber, there are [primitive and object]({{<relref "/docs/toc/data-types">}}) values. Primitives don't need any memory management, since they are copied by value and no heap allocation is required (with the exception of primitives being captured by a [closure]({{<relref "#closures">}})). 

Object are managed by ARC and each object has its own reference counter. Upon creating a new object, it receives a reference count of 1. When the object is copied, it's `retained` and the reference count increments by 1. When an object value is removed from it's parent or is no longer reachable in the current stack frame, it is `released` and the reference count decrements by 1.

Once the reference count reaches 0 and the object (eg. List or Map) also contains child references, each child reference is released thereby decrementing their reference counts by 1. Afterwards, the object is freed from memory.

### Optimizations.
The compiler can reduce the number of retain/release ops since it can infer value types even though they are dynamically typed to the user. Arguments passed to functions are only retained depending on the analysis from the callsite.

### Closures.
When primitive variables are captured by a [closure]({{<relref "/docs/toc/functions#closures">}}), they are boxed and allocated on the heap. This means they are managed by ARC and cleaned up when there are no more references to them.

### Fibers.
[Fibers]({{<relref "/docs/toc/concurrency#fibers">}}) are freed by ARC just like any other object. Once there are no references to the fiber, it begins to release it's child references by unwinding it's call stack.

## Heap.
Many object types in Cyber are small enough to be at or under 40 bytes. To take advantage of this, Cyber can reserve object pools to quickly allocate and free these small objects with very little bookkeeping. Bigger objects are allocated and managed by `mimalloc` which has proven to be a fast and reliable general-purpose heap allocator.

## Weak References.
> _Planned Feature_

## Cycle Detection.
The cycle detector is also considered a GC and frees abandoned objects managed by ARC. Although weak references can remove cycles altogether, Cyber does not force you to use them and provides a manual GC as a one-time catch all solution.
> _Incomplete Feature: Only the main fiber stack is cleaned up at the moment._

To invoke the GC, call the builtin function: `performGC`.
```cy
func foo():
  -- Create a reference cycle.
  var a = []
  var b = []
  a.append(b)
  b.append(a)

  var res = performGC()
  -- Cycle still alive in the current stack so no cleanup is done.
  print res['numCycFreed']    -- Output: 0
  print res['numObjFreed']    -- Output: 0

foo()
var res = performGC()
-- `a` and `b` are no longer reachable, so the GC does work.
print res['numCycFreed']      -- Output: 2
print res['numObjFreed']      -- Output: 2
```
