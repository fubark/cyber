---
title: "Memory"
bookFlatSection: true
weight: 12
---

# Memory.
Cyber provides memory safety by default.

## ARC.
Cyber uses ARC or automatic reference counting to manage memory.
ARC has less overhead compared to a tracing garbage collector and reduces GC pauses which makes Cyber suitable for realtime applications. ARC is also deterministic unlike tracing GCs which usually run on a separate thread(s).

### Reference Counting.
In Cyber, there are [primitive and object]({{<relref "/docs/toc/data-types">}}) values. Primitives don't need any memory management, since they are copied by value and no heap allocation is required (with the exception of primitives being captured by a [closure]({{<relref "#closures">}})). 

Objects are managed by ARC and each object has their own reference counter. Upon creating a new object, it receives a reference count of 1. When the object is copied, it's `retained` and the reference count increments by 1. When an object reference is no longer reachable in the current stack frame, it is `released` and the reference count decrements by 1. For example, at the end of a function call, all the local variables (that can potentially be an object reference) in the function are no longer reachable so they are released.

Once the reference count reaches 0 and the object (eg. lists and maps) also contains child references, each child reference is released thereby decrementing their reference counts by 1. Afterwards, the object is freed from memory.

### Optimizations.
Cyber's compiler can reduce the number of retain/release ops since it can infer value types even though they are dynamically typed to the user. Arguments passed to functions are only retained depending on the analysis from the callsite. ARC expressions do not retain temporary values unless it is required for the happy path. In the rare case that a temporary value is released before it is used, the value behaves as if it was the `none` value.

### Closures.
When primitive variables are captured by a [closure]({{<relref "/docs/toc/functions#closures">}}), they are boxed and allocated on the heap. This means they are managed by ARC and are cleaned up when there are no more references to them.

### Fibers.
[Fibers]({{<relref "/docs/toc/concurrency#fibers">}}) are freed by ARC just like any other object. Once there are no references to the fiber, it begins to release it's child references by unwinding its call stack.

## Heap.
Many object types in Cyber are small enough to be at or under 40 bytes. To take advantage of this, Cyber can reserve object pools to quickly allocate and free these small objects with very little bookkeeping. Bigger objects are allocated and managed by `mimalloc` which has proven to be a fast and reliable general purpose heap allocator.

## Weak Refs.

## Cycle Detection.