### iteration test

```
i64 sum = 0
i64 i = 0

i64 add(i64 a, i64 b) {
    a + b
}

while i < 10000 {
    sum = add(sum, i)
    i = i + 1
}

print sum
```
at iterations: `1k`, `5k`, `10k`, `100k`, `1m`
- `0.0.2`: `100ms`, `2.6s`, `32.7s`, `_`, `_`
- `0.0.3`: `2.7ms`, `10ms`, `17ms`, `150ms`, `1.48s`