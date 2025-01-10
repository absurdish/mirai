### iteration test

```
i32 sum = 0
i32 i = 0

i32 add(i32 a, i32 b) {
    a + b
}

while i < 10000 {
    sum = add(sum, i)
    i = i + 1
}

print sum
```
at iterations: `1k`, `5k`, `10k`
- `0.0.2`: `100ms`, `2.6ms`, `32.7ms`