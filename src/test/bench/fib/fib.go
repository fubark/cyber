package main

import "time"
import "fmt"

func fib(n int) int {
    if n < 2 {
        return n
    }
    return fib(n-1) + fib(n-2)
}

func main() {
    start := time.Now()
    res := fib(30)
    fmt.Printf("time: %f\n", float64(time.Since(start).Nanoseconds()) / 1000000.0)
    fmt.Println(res)
}
