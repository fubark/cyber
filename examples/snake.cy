-- Copyright (c) 2023 Cyber (See LICENSE)

-- Snake, a game ported from Raylib examples.
-- Check that `ray.cy` points to your platform's raylib library.

import os 'os'
import ray 'ray.cy'

var SNAKE_LENGTH = 256
var SQUARE_SIZE = 31

object Vec2:
    x number
    y number

object Snake:
    pos Vec2
    size Vec2
    speed Vec2
    color Color

object Food:
    pos Vec2
    size Vec2
    active bool
    color Color

var screenWidth = 800
var screenHeight = 450

var framesCounter = 0
var gameOver = false
var pause = false

var fruit = Food{}
var snake = arrayFill(Snake{}, SNAKE_LENGTH)
var snakePosition = arrayFill(Vec2{}, SNAKE_LENGTH)
var allowMove = false
var offset = Vec2{ x: 0, y: 0 }
var counterTail = 0

func main():
    ray.InitWindow(screenWidth, screenHeight, 'classic game: snake')
    InitGame()
    ray.SetTargetFPS(60)

    -- Main game loop
    while !ray.WindowShouldClose():  -- Detect window close button or ESC key
        UpdateGame()
        DrawGame()

    UnloadGame()    -- Unload loaded data (textures, sounds, models...)
    ray.CloseWindow()   -- Close window and OpenGL context

main()

-- Initialize game variables
func InitGame():
    static framesCounter = 0
    static gameOver = false
    static pause = false

    static counterTail = 1
    static allowMove = false

    offset.x = screenWidth % SQUARE_SIZE
    offset.y = screenHeight % SQUARE_SIZE

    for 0..SNAKE_LENGTH each i:
        snake[i].pos = Vec2{ x: offset.x/2, y: offset.y/2 }
        snake[i].size = Vec2{ x: SQUARE_SIZE, y: SQUARE_SIZE }
        snake[i].speed = Vec2{ x: SQUARE_SIZE, y: 0 }

        if i == 0:
            snake[i].color = ray.DARKBLUE
        else:
            snake[i].color = ray.BLUE

    for 0..SNAKE_LENGTH each i:
        snakePosition[i] = Vec2{ x: 0, y: 0 }

    fruit.size = Vec2{ x: SQUARE_SIZE, y: SQUARE_SIZE }
    fruit.color = ray.SKYBLUE
    fruit.active = false

func UpdateGame():
    static pause
    static allowMove
    static framesCounter
    static gameOver
    static counterTail

    if !gameOver:
        if ray.IsKeyPressed(asciiCode('P')):
            pause = !pause

        if !pause:
            -- Player control
            if ray.IsKeyPressed(ray.KEY_RIGHT) and snake[0].speed.x == 0 and allowMove:
                snake[0].speed = Vec2{ x: SQUARE_SIZE, y: 0 }
                allowMove = false
            if ray.IsKeyPressed(ray.KEY_LEFT) and snake[0].speed.x == 0 and allowMove:
                snake[0].speed = Vec2{ x: -SQUARE_SIZE, y: 0 }
                allowMove = false
            if ray.IsKeyPressed(ray.KEY_UP) and snake[0].speed.y == 0 and allowMove:
                snake[0].speed = Vec2{ x: 0, y: -SQUARE_SIZE }
                allowMove = false
            if ray.IsKeyPressed(ray.KEY_DOWN) and snake[0].speed.y == 0 and allowMove:
                snake[0].speed = Vec2{ x: 0, y: SQUARE_SIZE }
                allowMove = false

            -- Snake movement
            for 0..counterTail each i:
                snakePosition[i] = copy(snake[i].pos)

            if framesCounter % 5 == 0:
                for 0..counterTail each i:
                    if i == 0:
                        snake[0].pos.x += snake[0].speed.x
                        snake[0].pos.y += snake[0].speed.y
                        allowMove = true
                    else:
                        snake[i].pos = copy(snakePosition[i-1])

            -- Wall behaviour
            if snake[0].pos.x > screenWidth - offset.x or
                snake[0].pos.y > screenHeight - offset.y or
                snake[0].pos.x < 0 or snake[0].pos.y < 0:
                gameOver = true

            -- Collision with yourself
            for 1..counterTail each i:
                if snake[0].pos.x == snake[i].pos.x and
                    snake[0].pos.y == snake[i].pos.y:
                    gameOver = true

            -- Fruit position calculation
            if !fruit.active:
                fruit.active = true
                fruit.pos = Vec2{
                    x: ray.GetRandomValue(0, screenWidth/SQUARE_SIZE - 1) * SQUARE_SIZE + offset.x/2,
                    y: ray.GetRandomValue(0, screenHeight/SQUARE_SIZE - 1) * SQUARE_SIZE + offset.y/2
                }

                while:
                    fruit.pos = Vec2{
                        x: ray.GetRandomValue(0, screenWidth/SQUARE_SIZE - 1) * SQUARE_SIZE + offset.x/2,
                        y: ray.GetRandomValue(0, screenHeight/SQUARE_SIZE - 1) * SQUARE_SIZE + offset.y/2
                    }
                    hit = false
                    for 0..counterTail each i:
                        if fruit.pos.x == snake[i].pos.x and fruit.pos.y == snake[i].pos.y:
                            hit = true
                            break
                    if !hit:
                        break

            -- Collision
            if snake[0].pos.x < fruit.pos.x + fruit.size.x and
                snake[0].pos.x + snake[0].size.x > fruit.pos.x and 
                snake[0].pos.y < fruit.pos.y + fruit.size.y and
                snake[0].pos.y + snake[0].size.y > fruit.pos.y:
                snake[counterTail].pos = copy(snakePosition[counterTail - 1])
                counterTail += 1
                fruit.active = false

            framesCounter += 1

    else:
        if ray.IsKeyPressed(ray.KEY_ENTER):
            InitGame()
            gameOver = false

func DrawGame():
    ray.BeginDrawing()
    if os.system == 'macos':
        ray.DrawRectangle(0, 0, screenWidth, screenHeight, ray.RAYWHITE)
    else:
        ray.ClearBackground(ray.RAYWHITE)

    if !gameOver:
        -- Draw grid lines
        for 0..screenWidth/SQUARE_SIZE + 1 each i:
            ray.DrawLine(
                SQUARE_SIZE*i + offset.x/2, offset.y/2,
                SQUARE_SIZE*i + offset.x/2, screenHeight - offset.y/2, ray.LIGHTGRAY)

        for 0..screenHeight/SQUARE_SIZE + 1 each i:
            ray.DrawLine(
                offset.x/2, SQUARE_SIZE*i + offset.y/2,
                screenWidth - offset.x/2, SQUARE_SIZE*i + offset.y/2, ray.LIGHTGRAY)

        -- Draw snake
        for 0..counterTail each i:
            ray.DrawRectangle(snake[i].pos.x, snake[i].pos.y, snake[i].size.x, snake[i].size.y, snake[i].color)

        -- Draw fruit to pick
        ray.DrawRectangle(fruit.pos.x, fruit.pos.y, fruit.size.x, fruit.size.y, fruit.color)

        if pause:
            ray.DrawText('GAME PAUSED', screenWidth/2 - ray.MeasureText('GAME PAUSED', 40)/2, screenHeight/2 - 40, 40, ray.GRAY)
    else:
        ray.DrawText('PRESS [ENTER] TO PLAY AGAIN', ray.GetScreenWidth()/2 - ray.MeasureText('PRESS [ENTER] TO PLAY AGAIN', 20)/2, ray.GetScreenHeight()/2 - 50, 20, ray.GRAY)

    ray.EndDrawing()

func UnloadGame():
    -- TODO: Unload all dynamic loaded data (textures, sounds, models...)
    pass
