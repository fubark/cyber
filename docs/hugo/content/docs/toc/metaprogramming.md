---
title: "Metaprogramming"
bookFlatSection: true
weight: 10
---

# Metaprogramming.

## Operator overloading.

## Custom operators.

## Magic functions.

### Calling a module symbol.
Declare a `<call>` function to allow invoking a module as a function. Currently, this is only available to builtin types like `float`.
```cy
-- Type declarations are also modules.
type Vec2 object:
  x float
  y float

  func <call>(x float, y float) Vec2:
    return Vec2{ x: x, y: y }

var v = Vec2(1, 2)
```

## Reflection.
A `metatype` object references an internal type. Use the `typeof` builtin to get the `metatype` of a value.
```cy
var val = 123
print typeof(val)   -- 'type: float'

-- Referencing a type as a value also returns its `metatype`.
print boolean       -- 'type: boolean'
```

### `type metatype`
```cy
func id(self) float
-- Returns the type ID as a float.
```

## Annotations.

## Runtime eval.

## Generics.

## Compile-time.