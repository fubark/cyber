---
title: Introduction
type: docs
---

# Introduction.

Cyber is a fast, efficient, and concurrent scripting language. To learn more about Cyber itself visit the website at [cyberscript.dev](https://cyberscript.dev).

Cyber is easy to learn. These docs provide a reference manual for the language. You can read it in order or jump around using the navigation.

You may come across features that are marked `Incomplete` or `Planned`. This is because the docs are written as if all features have been completed already.

## Hello World.
```cy
import math

var worlds = ['World', '世界', 'दुनिया', 'mundo']
worlds.append(math.random())
for worlds -> w:
    print 'Hello, $(w)!'
```