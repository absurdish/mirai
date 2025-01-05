statements and some exprs:
var: `type id = expr`, `id = expr`, `pub type id = expr` // inference if no type provided
assign: `id = expr` // interpreter will identify if the statement is declaration or assignement
call var: `id`
func def: `type id(type id, type id){}`, `id(){}` // no type required for void
func call: `id(expr, expr)`, `id: expr`, `id;` // alternative method for single arguments, and argumentless funcs are called like vars with `;`
func anon: `type anon(){}`
func pub: `pub type id(){}`
return: `return expr`
if/else: `if expr stmt`, `if expr stmt else stmt`
block stmt: `{}`
while: `while expr stmt`
for: `for stmt; expr; expr; stmt`
for/in: `for type id in expr stmt`
use: `use id` package (std/lib), `use id::id` file in project, `use 'href'` dynamic import, `use id from id` import fn, var, type... from source
type: `type id = texpr` `type` is keyword here, not placeholder and `texpr` is type expression
enum: `enum Id {Id, type Id, type Id expr}`
enum call: `Id::Id`, `Id::Id(expr)`
macros: `#id`

```
// example
int count = 0 // no semicolons are used, algorithm will detect closure by new line or a new statement
name = 'john' // declaration
count = 1 // assignmenet
print: count
int add(int a, int b){a + b} // supports both auto and manual returns
main(){}
add(count, 4)
main;
{
    // locally declared
    int x = 1
}
if count > 0 {}
if count > 2 {} else {}
while true {}
for int i = 0; i < 10; i ++ {}
int[] items = [1, 2, 3, 4]
for int i in items {}
type str_or_int = str | int
enum Fruit {Apple, int Grape, int Banana 0}
Fruit::Apple
Fruit::Grape(1) // error if no argument
Fruit::Banana // returns 0 if no argument
#no-auto-return
int two_plus_two(){return 2 + 2}
```
