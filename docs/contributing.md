## Contributing
*This document is a work in progress.*

## Using Cyber.
The best way to get started with Cyber is to try using it for scripting or for a project. This lets you get a taste of the language and in the process learn more about it. If you run into a bug or notice something that is inconsistent with the [docs](https://fubark.github.io/cyber), please report an issue.

## Building Cyber.
The next step you might take is building Cyber on your own machine. You'll learn how to test and build the source which is useful if you want to make code contributions. See [Building](https://github.com/fubark/cyber/blob/master/docs/build.md).

Cyber is written in Zig. One way to learn Zig is to fix some [problem sets](https://github.com/ratfactor/ziglings). If you know C, you'll pick up Zig quickly.

## Language Design.
After v1.0 of Cyber, the language won't be changing much, at least that is the goal. So if you have a feature idea for the language or you don't like how something is designed, now would be a good time to propose it. You can use the [docs](https://fubark.github.io/cyber) as a canonical reference to the current language features.

## Improving the VM.
Working on the compiler and interpreter can be tricky since any change can have a noticeable impact on performance and Cyber wants to be fast. For that reason, you'd need to have a better understanding of the internals. In the future, we'll have a dedicated doc describing how Cyber compiles a script and evaluates it. For now, you can look at the source code which is divided into their areas of concern. Docs and notes are usually placed next to declared data structures so you can grok the design without reading a lot of code.

Another useful thing to know is how to run the benchmarks. `hyperfine` is a nice tool for this and if you want to measure a specific part of your script you can use the `milliTime()` function in Cyber's `os` module. 

