## Build
To get the maximum performance, you'll need to build Cyber from source.

## Dependencies
Get the latest Zig compiler (0.11.0-dev) [here](https://ziglang.org/download/).
If you use Zig often, you can use [zigup](https://github.com/marler8997/zigup) to manage different versions of the compiler.

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
zig build wasm-test -Drelease-fast -Dtarget=wasm32-freestanding
wasm3 zig-out/test/test.wasm
```

## Build the CLI.
Cyber is optimized for the release-fast build.
```sh
# For your native target.
zig build cli -Drelease-fast

# For cross platform build. eg. Host: Linux x64, Target: MacOS arm64
zig build cli -Drelease-fast -Dtarget=aarch64-macos.12-none

# For a debug build instead.
zig build cli
```

If all goes well, the Cyber CLI executable will be located at `./zig-out/cyber/cyber`.

## Build as a Library.
When using Cyber as a API library, you'll need to build a library instead.
```sh
# For your native target.
zig build lib -Drelease-fast

# For cross platform build. eg. Host: Linux x64, Target: MacOS arm64
zig build lib -Drelease-fast -Dtarget=aarch64-macos.12-none

# For Web/WASM.
zig build lib -Drelease-fast -Dtarget=wasm32-freestanding
```

## Troubleshooting.
- If you have trouble building mimalloc on MacOS, consider changing the sdk path in `lib/mimalloc/lib.zig` to your installed version.

- On a linux distro with selinux (eg. Fedora), add `-Dselinux` to the zig build commands.
