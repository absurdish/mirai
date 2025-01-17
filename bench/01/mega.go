package main

import "fmt"

func add(a, b int64) int64 {
    return a + b
}

func main() {
    var sum int64 = 0
    var i int64 = 0

    for i < 10000000 {
        sum = add(sum, i)
        i++
    }

    fmt.Println(sum)
}
