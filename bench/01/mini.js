function add(a, b) {
    return a + b;
}

let sum = 0;
let i = 0;

while (i < 100000) {
    sum = add(sum, i);
    i++;
}

console.log(sum);
