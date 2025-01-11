### 1. simple arithmetics

```
i64 sum = 0
i64 i = 0

i64 add(i64 a, i64 b) {
    a + b
}

while i < 100000 {
    sum = add(sum, i)
    i = i + 1
}

print sum
```

- mini: `100k` iterations
- norm: `1m` iterations
- mega: `10m` iterations

### 2. nested loop

```
i64 sum = 0
i64 i = 0
int j = 0

while i < 100 {
    while j < 100 {
        sum = sum + i + j
        j = 1 + j
    }
    i = i + 1
    j = 0
}

print sum
```

- mini: `100-100` iterations
- norm: `1k-1k` iterations
- mega: `10k-1k` iterations

| version |   1. arithmetics    |  2. nested loops   |
|:--------|:-------------------:|:------------------:|
| 0.0.3   | 150ms, 1.48s, 14.6s | 11ms, 900ms, 9.1s  |
| 0.0.4   | 157ms,  1.6s, 15.9s | 12ms, 950ms,  9.5s |
|         |                     |                    |

