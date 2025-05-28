This is an example NDK project that shows how to build a main.c that links with `libcyber.a` and runs on an emulator.

## Set up Android environment.
1. Download `sdkmanager` from the command-line tools: https://developer.android.com/studio#command-tools
2. List packages: `sdkmanager --list`
3. Install NDK: `sdkmanager "ndk;27.0.12077973"`
4. Install platform: `sdkmanager "platforms;android-34"`
5. Install platform tools: `sdkmanager platform-tool`
6. Install AVD image: `sdkmanager "system-images;android-34;default;arm64-v8a"`
7. Create AVD: `avdmanager create avd -n my_avd -k "system-images;android-34;default;arm64-v8a"`

## Build libcyber and the project.
1. Zig 0.14.1 release
2. Build libcyber (from the cyber repo directory): `zig build lib -Doptimize=ReleaseFast -Dtarget=aarch64-linux-android -Disystem="/path/to/android_sdk/ndk/27.0.12077973/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/include" -Disystem="/path/to/android_sdk/ndk/27.0.12077973/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/include/aarch64-linux-android"`
The `-Disystem` flags tells Zig where the libc headers are for the android target.
3. Copy to this project: `cp zig-out/lib/libcyber.a examples/android/ndk_app/`
4. Build project (in this directory): `NDK_PROJECT_PATH=. ndk-build NDK_APPLICATION_MK=./Application.mk`

## Run in emulator.
1. List AVDs: `emulator -list-avds`
2. Start emulator: `emulator -avd my_avd`
3. List devices: `adb devices`
4. Start adb shell: `adb -s emulator-5554 shell`
5. Push app binary: `adb push obj/local/arm64-v8a/main.out /sdcard`
6. Sudo in shell: `su`
7. Copy to `/data/local` and add execute permissions: `cp /sdcard/main.out /data/local/main.out && chmod a+x /data/local/main.out`
8. Run app: `/data/local/main.out`
