def add(a, b):
    return a + b

sum = 0
i = 0

while i < 10000000:
    sum = add(sum, i)
    i += 1

print(sum)
