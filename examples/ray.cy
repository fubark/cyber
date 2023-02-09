-- Copyright (c) 2023 Cyber (See LICENSE)

import os 'os'

-- First, download the raylib library for your platform:
-- https://github.com/raysan5/raylib/releases
-- Next, change `path` to the location of the library.

-- var path = '/home/fubar/Downloads/raylib-4.2.0_linux_amd64/lib/libraylib.so'
var path = '/Users/fubar/Downloads/raylib-4.2.0_macos/lib/libraylib.dylib'
-- var path = 'C:/Users/fubar/Downloads/raylib-4.2.0_win64_msvc16/lib/raylib.dll'

export func InitWindow(width, height, title) = lib.InitWindow
export func CloseWindow() = lib.CloseWindow
export func SetTargetFPS(fps) = lib.SetTargetFPS
export func WindowShouldClose() = lib.WindowShouldClose
export func IsKeyPressed(key) = lib.IsKeyPressed
export func GetRandomValue(start, end) = lib.GetRandomValue
export func BeginDrawing() = lib.BeginDrawing
export func EndDrawing() = lib.EndDrawing
export func GetScreenWidth() = lib.GetScreenWidth
export func GetScreenHeight() = lib.GetScreenHeight
export func ClearBackground(color) = lib.ClearBackground
export func DrawLine(x, y, x2, y2, color) = lib.DrawLine
export func DrawRectangle(x, y, width, height, color) = lib.DrawRectangle
export func MeasureText(text, size) = lib.MeasureText
export func DrawText(text, x, y, size, color) = lib.DrawText

var lib = try bindLib(path, [
    CFunc{ sym: 'InitWindow', args: [#int, #int, #charPtrZ], ret: #void }
    CFunc{ sym: 'CloseWindow', args: [], ret: #void }
    CFunc{ sym: 'SetTargetFPS', args: [#int], ret: #void }
    CFunc{ sym: 'WindowShouldClose', args: [], ret: #bool }
    CFunc{ sym: 'IsKeyPressed', args: [#int], ret: #bool }
    CFunc{ sym: 'GetRandomValue', args: [#int, #int], ret: #int }
    CFunc{ sym: 'BeginDrawing', args: [], ret: #void }
    CFunc{ sym: 'EndDrawing', args: [], ret: #void }
    CFunc{ sym: 'GetScreenWidth', args: [], ret: #int }
    CFunc{ sym: 'GetScreenHeight', args: [], ret: #int }
    CFunc{ sym: 'ClearBackground', args: [#uint], ret: #void }
    CFunc{ sym: 'DrawLine', args: [#int, #int, #int, #int, #int], ret: #void }
    CFunc{ sym: 'DrawRectangle', args: [#int, #int, #int, #int, #int], ret: #void }
    CFunc{ sym: 'MeasureText', args: [#charPtrZ, #int], ret: #int }
    CFunc{ sym: 'DrawText', args: [#charPtrZ, #int, #int, #int, #int], ret: #void }
], { genMap: true })

export func toColor(r, g, b, a):
    if os.endian == #little:
        return r | g << 8 | b << 16 | a << 24
    else:
        return a | b << 8 | g << 16 | r << 24

export var RAYWHITE = toColor(245, 245, 245, 255)
export var GRAY = toColor(130, 130, 130, 255)
export var LIGHTGRAY = toColor(200, 200, 200, 255)
export var BLUE = toColor(0, 121, 241, 255)
export var DARKBLUE = toColor(0, 82, 172, 255)
export var SKYBLUE = toColor(102, 191, 255, 255)

export var KEY_RIGHT = 262
export var KEY_LEFT = 263
export var KEY_DOWN = 264
export var KEY_UP = 265
export var KEY_ENTER = 257