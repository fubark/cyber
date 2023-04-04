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
Declare a `<call>` function to allow invoking a module as a function. Currently, this is only available to builtin types like `number`.
```cy
-- Type declarations are also modules.
type Vec2 object:
  x number
  y number

  func <call>(x number, y number) Vec2:
    return Vec2{ x: x, y: y }

v = Vec2(1, 2)
```

## Reflection.
A `metatype` object references an internal type. Use the `typeof` builtin to get the `metatype` of a value.
```cy
val = 123
print typeof(val)   -- 'type: number'

-- Referencing a type as a value also returns its `metatype`.
print boolean       -- 'type: boolean'
```

### `type metatype`
```cy
func id(self) number
-- Returns the type ID as a number.
```

## Annotations.

## Runtime eval.

## Generics.

## Compile-time.