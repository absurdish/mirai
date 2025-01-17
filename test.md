# benchmarks

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

| language       | mini  | norm  | mega  |
| :------------- | :---: | :---: | :---: |
| python         | 25ms  | 130ms |  1s   |
| go             | 95ms  | 97ms | 100ms |
| javascript     | 35ms  | 35ms  | 40ms  |
| 0.0.4          | 120ms |  1s   |  10s  |
| 0.0.5 [latest] |  3ms  |  3ms  |  3ms  |

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
