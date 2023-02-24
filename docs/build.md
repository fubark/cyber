## Build
To get the maximum performance, you'll need to build Cyber from source.

## Dependencies
Get the Zig compiler (0.11.0-dev.1797+d3c9bfada) here:
- [Linux x64](https://ziglang.org/builds/zig-linux-x86_64-0.11.0-dev.1797+d3c9bfada.tar.xz)
- [macOS x64](https://ziglang.org/builds/zig-macos-x86_64-0.11.0-dev.1797+d3c9bfada.tar.xz)
- [macOS arm64](https://ziglang.org/builds/zig-macos-aarch64-0.11.0-dev.1797+d3c9bfada.tar.xz)
- [Windows x64](https://ziglang.org/builds/zig-windows-x86_64-0.11.0-dev.1797+d3c9bfada.zip)
- If you use Zig often, you can use [zigup](https://github.com/marler8997/zigup) to manage different versions of the compiler.

Once you have zig installed, checkout the repo:
```sh
git clone https://github.com/fubark/cyber.git
```

## Run Tests
```sh
# Runs all tests.
zig build test

# Runs just the tracing tests.
zig build test-trace

# WASM tests. For WASM target only.
zig build wasm-test -Doptimize=ReleaseFast -Dtarget=wasm32-freestanding
wasm3 zig-out/test/test.wasm
```

## Build the CLI.
Cyber is optimized for the ReleaseFast build.
```sh
# For your native target.
zig build cli -Doptimize=ReleaseFast

# For cross platform build. eg. Host: Linux x64, Target: MacOS arm64
zig build cli -Doptimize=ReleaseFast -Dtarget=aarch64-macos.12-none

# For a debug build instead.
zig build cli
```

If all goes well, the Cyber CLI executable will be located in `./zig-out/cyber`.

## Build as a Library.
When using Cyber as a API library, you'll need to build a library instead.
```sh
# For your native target.
zig build lib -Doptimize=ReleaseFast

# For cross platform build. eg. Host: Linux x64, Target: MacOS arm64
zig build lib -Doptimize=ReleaseFast -Dtarget=aarch64-macos.12-none

# For Web/WASM.
zig build lib -Doptimize=ReleaseFast -Dtarget=wasm32-freestanding
```

You'll find the resulting shared library in `./zig-out/lib`.

## Troubleshooting.
- If you have trouble building mimalloc on MacOS, consider changing the sdk path in `lib/mimalloc/lib.zig` to your installed version.

- On a linux distro with selinux (eg. Fedora), add `-Dselinux` to the zig build commands.
