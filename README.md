# OmniDice: Dice Probability Calculator, Supercharged

OmniDice is a scripting language conceptually based on [AnyDice](https://anydice.com), with expanded syntax based on a simplified version of Rust. It was created as a tool for performing more complex computations than AnyDice is capable of handling, in a way that is more legible to those who can understand other commonly used programming languages.

# Features

## Datatypes
OmniDice contains five fundamental datatypes (int, float, bool, string, and dice), and several structures that use them. 
### Int
The simplest type, integers (ints, or whole numbers) support numerical operations such as addition (`+`), subtraction (`-`), multiplication (`*`), division (`/`), negation (`-` as a prefix), and modulo (`%`). They also support boolean operations such as equality (`==`), inequality (`!=`), greater than (`>`), less than (`<`), greater than or equal to (`>=`), and less than or equal to (`<=`). Ints in OmniDice are represented as 32-bit signed integers internally, and are written as-is, eg. `345` or `-12`.
### Float
Floats (floating point numbers, or decimals) support most of the same operations as Ints, with the exception of modulo. Either side of any float-based operation can be replaced with an int for the same result (eg. `4.5+2.0` will give the same result as `4.5+2`, with the `2` being read as an int rather than a float). Floats in OmniDice are represented internally with 32 bits, the same size as the integers, and are written the same way as integers with the additional restriction that they must contain exactly one decimal, eg. `-435.356`.
### Bool
Boolean values (or bools) support all common operations, such as and (`&&`), or (`||`), equal (`==`), not equal (`!=`), and not (`!` as a prefix). They have only two potential states, `true` and `false`, which can be written as-is in code.
### String
Strings support equality and inequality operations, as well as concatenation using `+`. They are also the first type in OmniDice to have properties attached to them, which are accessed using the `.` operator. Strings only contain a single property, called `length`, which is an integer. They can be written as any sequence of characters surrounded by quotation marks (`"`), and allow escape characters such as `\n` for newline and `\"` for a non-terminating quotation mark.
### Dice
Dice are the last and most important basic OmniDice type, consisting of a set of integer values and associated probabilities. They allow all of the numerical operations that an integer does (again, with the exception of modulo) either between themselves (resulting in the operation being performed across all possible pairs of results) or between one dice and one integer (resulting in the integer being operated with every possible result on the dice). They also contain several properties, namely `max` and `min`, which are integers, and `mean`, which is a float. They can be written in standard dice notation, ie. `###d###` or `d###`, so statements such as `2d8+d6+2` are valid OmniDice code.
### Array
Arrays are the first of the complex types that OmniDice supports, and can contain any number of a single type of value. They support only one operation, which is concatenation via +, but they contain multiple properties, including length (an integer) as well as `push`, `pop`, and `iter` (functions, with `push` accepting an element to add, `pop` removing and returning the last one, and `iter` returning an iterator over the array). They can be constructed using square brackets (\[/]), so an array of integers could be defined by writing `[1,2,3,4+5,16]`. They can also be indexed with square brackets (eg. `my_array[1]`) and can index multiple values simultaneously, which will return an array (eg. `[1,6,2,7,3][2,3]` will yield `[2,7]`).
### Tuple
Tuples are similar in principle to arrays, except that they have a fixed size and each element can store a different type, and are written with normal brackets. For example, you could construct a tuple containing a string, an integer, and a die using the expression `("str", 42, 3d8+5)`. These values can be retrieved using properties, with the name of the property being the index of the element to retrieve preceded by an `i` (e.g. to retrieve the second element of a tuple, you would access the property `(tuple).i1`).
### Maybe
Maybe is a type that contains either a single value (called the `Filled` state) or no value (called `Null`). This type is OmniDice's stand-in for null values, in keeping with Rust's Option type. They cannot currently be constructed explicitly (
TODO: make a constructor for both variants of Maybe as a builtin
) but they can be returned from iterators (
TODO: make array pop return a maybe
). Maybe has two properties, `unwrap` (a function that returns the filled value, or errors if null) and `filled` (a boolean, `true` if filled, `false` if null).
### Iter
Iterators are OmniDice's answer to for loops, and work similarly to Rust's iterators, with some functions removed for simplicity. The functions that remain are next, which returns a maybe containing the next element of the iterator or null if it's finished, map, which accepts a function and returns a new iterator with all previous values passed through the function, and iter, which returns the iterator itself (for compatibility with for loops). They cannot be constructed explicitly either, but they are returned from values that can be iterated over, such as arrays and ranges.
### Range
Range is a utility type for more easily constructing iterators through a range of integers. They are written as `###..###`, eg. `0..10`, and have an `iter` function that returns an iterator over that set of values (currently inclusive,
TODO: make ranges not inclusive of the endpoint). They contain a single function, `iter`, whose signature is the same as that of an array and returns an iterator over the range specified.
### Function
Functions are the most complex type in OmniDice, though they contain few operations or properties. The main method of interacting with functions is to call them, which is done using regular brackets containing whatever parameters the function requires. Functions can be defined using the following syntax:
```
func(parameter_name: type, other_parameter_name: other_type) -> return_type {
    function_body_expressions (the last one contains the return value)
}
```
### Ref
Ref is a type that allows mutable access to normally pass-by-value types, ie. every type excluding arrays and tuples. They can be constructed by passing a value into the builtin `ref` function, and the internal value can be accessed using the `.inner` property. An example of this at work is as follows:
```
let a = ref(7);
let b = 7;
let f = func() -> () {
    a.inner += 7;
    b += 7;
};
f();
println("a={}, b={}", a.inner, b); // This prints 'a=14, b=7', since the value of b is copied but a is referenced inside the function.
```
### Void
Void is a utility type used when a type is needed, but no value needs to be put there (for example when a function doesn't need to return a value). It can be constructed with a pair of brackets, ie. `()`.

## Builtins
OmniDice comes with several builtin functions not directly connected to any value, including println, error, and format. There are others, but they're primarily for constructing types previously mentioned and so they are not described here.
### println
The `println` function is used to output data to the console. It can be called with at least one parameter (a string) and any number of additional values to insert into the string at points denoted by `{}`. As an example, evaluating `println("first: {}, second: {}", "abc123", 17.4)` would print `first: abc123, second: 17.4` to the console, followed by a new line.
### error
`error` accepts any number of values and returns (in theory) void. However, when called, error causes the program to shut down with an error whose contents are whatever arguments were passed into it.
### format
`format` operates exactly the same as `println`, except instead of printing the result to the console, it returns it as a string.

## Control Flow
OmniDice contains several useful control flow constructs, such as conditional blocks (if/else), for and while loops, and functions.

### Conditionals
The syntax for conditional statements is the same as it is in Rust. Such statements always start with `if [condition] {[result]}`, and may optionally continue with `else if [alternate condition] {[alternate result]}` and/or `else {[final result]}`. All result expressions must end with a line that returns void (or a semicolon), unless they end with a conditionless else block, in which case it is permitted that all results evaluate to the same non-void type, causing the conditional expression as a whole to take on the value of whichever result is used.

### For loops
For loops use the `iter` type described earlier, and their syntax is as follows: `for [variable] in [iter] {[body]}`. The body expression must return void (or end with a semicolon), and the iterator must have a function named iter which returns an iterator (currently, `iter`, `range`, and `array` are the only types which do this).

### While loops
While loops' syntax is as follows: `while [condition] {[body]}`. The condition must evaluate to a boolean, and the body (which must evaluate to void) will continue to be run until the condition is false.

### Functions
For a description of functions, see their entry under types, as they are first-class types and can be used in the same way as any other.