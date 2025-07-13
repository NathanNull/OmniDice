# OmniDice: Dice Probability Calculator, Supercharged

OmniDice is a scripting language conceptually based on [AnyDice](https://anydice.com), with expanded syntax based on a simplified version of Rust. It was created as a tool for performing more complex computations than AnyDice is capable of handling, in a way that is more legible to those who can understand other commonly used programming languages.

# Features

## Datatypes
OmniDice contains five fundamental datatypes (int, float, bool, string, and dice), and several structures that use them. 


### Int
#### Overview
The simplest type, integers (ints, or whole numbers) support numerical operations such as addition (`+`), subtraction (`-`), multiplication (`*`), division (`/`), negation (`-` as a prefix), and modulo (`%`). They also support boolean
operations such as equality (`==`), inequality (`!=`), greater than (`>`), less than (`<`), greater than or equal to (`>=`), and less than or equal to (`<=`). Ints in OmniDice are represented as 32-bit signed integers internally, and are
written as-is, eg. `345` or `-12`.
#### Example
```
printf("{}", 1 + 2 - 3 * 4 / (5 / -2)); // prints 9
printf("{}", 1 >= 2); // prints false
```


### Float
#### Overview
Floats (floating point numbers, or decimals) support most of the same operations as Ints, with the exception of modulo. Either side of any float-based operation can be replaced with an int for the same result (eg. `4.5+2.0` will give the
same result as `4.5+2`, with the `2` being read as an int rather than a float). Floats in OmniDice are represented internally with 32 bits, the same size as the integers, and are written the same way as integers with the additional
restriction that they must contain exactly one decimal, eg. `-435.356`. Floats that could be represented as a whole number can be written with `.0` on the end, eg. `7.0`, or with only a decimal point, eg. `7.`.
#### Example
```
printf("{}", 12.0 - 13.5); // prints -1.5
```


### Bool
#### Overview
Boolean values (or bools) support most common operations, such as and (`&&`), or (`||`), equal (`==`), not equal (`!=`), and not (`!` as a prefix). They have only two potential states, `true` and `false`, which can be written as-is in code.
#### Example
```
printf("{}", false || !false); // prints true
```


### String
#### Overview
Strings support equality and inequality operations, as well as concatenation using `+`. They are also the first type in OmniDice to have properties attached to them, which are accessed using the `.` operator. Strings only contain a single
property, called `length`, which is a function which returns an integer. They can be written as any sequence of characters surrounded by quotation marks (`"`), and allow some escape characters such as `\n` for newline and `\"` for a
non-terminating quotation mark.
#### Example
```
printf("{}", ("firststring"+"secondstring").length()); // prints 23
```


### Dice
#### Overview
Dice are the last and most important basic OmniDice type, consisting of a set of integer values (possible rolls) and associated probabilities. They allow all of the numerical operations that an integer does (with the exception of modulo)
either between themselves (resulting in the operation being performed across all possible pairs of results) or between one dice and one integer (resulting in the integer being operated with every possible result on the dice). They also
contain several properties, namely `max` and `min`, which are integers, and `mean` and `stddev` (standard deviation, a measure of how spread apart a roll's outcomes are), which is a float. They can be written in standard dice notation, ie.
`#d#` or `d#`, so statements such as `2d8+d6+2` are valid OmniDice code.
#### Example
```
printf("{}", (1d8+5).mean); // prints 9.5
```


### Array
#### Overview
Arrays are the first of the complex types that OmniDice supports, and can contain any number of a single type of value. They support only one operation, which is concatenation via +, but they contain multiple properties, including length
(a function that returns an integer, like strings have) as well as `push`, `pop`, and `iter` (functions, with `push` accepting an element to add, `pop` removing and returning the last one (inside a maybe), and `iter` returning an iterator
over the array). They can be constructed using square brackets (\[/]), so an array of integers could be defined by writing `[1,2,3,4+5,16]`. They can also be indexed with square brackets (eg. `my_array[1]`) and can index multiple values
simultaneously, which will return an array.

When constructing an array that is initially empty, make sure that its type is explicitly written out somewhere nearby (often in the type of the variable you're assigning it to), or else the code will not compile.
#### Example
```
let a: [string] = []; // without the typehint, this would not compile
printf("{}", [1,6,2,7,3][2,3]); // prints [2,7]
```


### Tuple
#### Overview
Tuples are similar in principle to arrays, except that they have a fixed size and each element can store a different type, and are written with normal brackets. For example, you could construct a tuple containing a string, an integer, and a die using the expression `("str", 42, 3d8+5)`. These values can be retrieved using properties, with the name of the property being the index of the element to retrieve preceded by an `i` (e.g. to retrieve the second element of a tuple, you would access the property `(tuple).i1`).
#### Example
```
printf("{}", (1,2,[1,2,3],4,5).i2); // Prints [1,2,3]
```


### Maybe
#### Overview
Maybe is a type that contains either a single value (called the `Filled` state) or no value (called `Null`). This type is OmniDice's stand-in for null values, in keeping with Rust's Option type. They can be constructed using the filled(val) and null() functions. Maybe has two properties, `unwrap` (a function that returns the filled value, or errors if null) and `filled` (a boolean, `true` if filled, `false` if null).
#### Example
```
let a = filled(1d6);
printf("{}", a.unwrap());
```


### Iter
#### Overview
Iterators are OmniDice's answer to for loops, and work similarly to Rust's iterators, with some functions removed for simplicity. The functions that remain are `next`, which returns a maybe containing the next element of the iterator or null if it's finished, `map`, which accepts a function and returns a new iterator with all previous values passed through the function, `filter`, which accepts a function and returns a new iterator with only the values which cause that function to return true, `fold`, which accepts an initial value and a function to update that value for each element in the iterator, and `iter`, which returns the iterator itself (for compatibility with for loops). They can be constructed explicitly using the iter(f) function, which accepts a function to take values from, but they are more commonly returned from values that can be iterated over, such as arrays and ranges.
#### Example
```
let it = (1..5).iter();
for next in iter(func() -> maybe<int> {
    // rough equivalent of 'if let' syntax from Rust
    if (let n_opt = it.next()).filled {
        let next = n_opt.unwrap();
        filled(next * next)
    } else {
        null()
    }
}).map(func(param: int) -> int {param + param}) {
    printf("{}", next);
}
// prints 2, then 8, then 18, then 32.
```


### Range
#### Overview
Range is a utility type for more easily constructing iterators through a range of integers. They are written as `#..#`, eg. `0..10`, and have an `iter` function that returns an iterator over that set of values (including the start but not the end). They contain a single function, `iter`, whose signature is the same as that of an array and returns an iterator over the range specified.

There is an alternative syntax for constructing ranges that causes them to include both of their endpoints, which looks like `#..=#`, eg. `0..=10`.
#### Example
```
for a in 0..=4 {
    printf("{}", a);
}
// prints 0, then 1, then 2, then 3, then 4.
```


### Function
#### Overview
Functions are the most complex type in OmniDice, though they contain few operations or properties. The main method of interacting with functions is to call them, which is done using round brackets containing the parameters the function requires. Functions can be defined using the following syntax:
```
func(parameter_name: type, other_parameter_name: other_type) -> return_type {
    function; body; expressions (the last one contains the return value)
}
```
Functions can also be generic, allowing them to operate on multiple possible inputs. This is accomplished using the following syntax:
```
func<A, B, (...)>(parameter_name: type, other_parameter_name: other_type) -> return_type {
    function; body; expressions (the last one contains the return value)
}
```
Additionally, functions are the only place where the use of the `return` keyword is valid. This can be used to cause the function to return early. The keyword works the same as it does in most other languages, with the exception that it (as with most other expressions in this language) can be placed inside any other expression. This means that an expression like `filled(return 1d6)` is technically valid, though generally unadvisable.
#### Example
```
let adder = func<T>(a: T, b: T) -> T {
    a+b
};

printf("{}", adder(1,3)); // prints 4
printf("{}", adder(1.0,3.5)); // prints 4.5
printf("{}", adder(1d6,3d6)); // prints a display representing 4d6
```

### Ref
#### Overview
Ref is a type that allows mutable access to normally pass-by-value types, ie. every type excluding arrays and tuples. They can be constructed by passing a value into the builtin `ref` function, and the internal value can be accessed using the `.inner` property.

Refs of functions can also be used to do recursion (since functions are defined exclusively at runtime, trying to do it without refs requires accessing variables before they're defined), but how that works is left as an exercise to the reader.
#### Example
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
#### Overview
Void is a utility type used when a type is needed, but no value needs to be put there (for example when a function doesn't need to return a value). It can be constructed with a pair of brackets, ie. `()`, and it is returned from any function (builtin or user-defined) for which no other return type is specified.
#### Example
```
let do_nothing = func() {};
printf("{}", do_nothing()); // prints ()
```


## Builtins
OmniDice comes with several builtin functions not directly connected to any value, including println, printf, error, format, and dicemap. There are others, but they're primarily for constructing types previously mentioned and so they are not described here.
### println
#### Overview
The `println` function is used to output data to the console. It accept a single string parameter and outputs it followed by a newline.
#### Example
```
printf("Hello, world!"); // prints Hello, world!
```


### printf
#### Overview
The `printf` function is similar to `println`, but it accepts formatting arguments. It can be called with a string and at least one additional value to insert into the string at points denoted by `{}` (if more than one value is required, pass all values in together as a tuple).
#### Example
```
printf("first: {}, second: {}", ("abc123", 17.4)); // prints "first: abc123, second: 17.4"
```


### error
#### Overview
`error` accepts a single value and returns (in theory) void. However, when called, error causes the program to shut down with an error whose contents are whatever arguments were passed into it. It is templated to return Void, but in reality the code will never reach that point.
#### Example
```
error("Your code is bad"); // Crashes the program and reports an error message in the console.
```


### format
#### Overview
`format` operates exactly the same as `println`, except instead of printing the result to the console, it returns it as a string.
#### Example
```
let str = format("{}, {}", ("one argument", "another"));
println(str);
```

### dicemap
#### Overview
`dicemap` is the most complex builtin function, but it also has the most versatility. It accepts an array of dice and a function as arguments, with the function accepting an array of integers of the same size and returning a single integer. When called, `dicemap` runs the given function on all possible combinations of rolls that the given dice could produce, and returns a new dice containing all of those results, weighted by the odds of each combination showing up.
#### Example
```
let advantage = dicemap([1d20, 1d20], func(rolls: [int]) -> int {
    if rolls[0] > rolls[1] {
        rolls[0]
    } else {
        rolls[1]
    }
});
printf("{}", advantage); // prints a display of a d20 rolled with advantage.
```


## Control Flow
OmniDice contains several useful control flow constructs, such as conditional blocks (if/else), for and while loops, and functions.

### Conditionals
#### Overview
The syntax for conditional statements is the same as it is in Rust. Such statements always start with `if [condition] {[result]}`, and may optionally continue with `else if [alternate condition] {[alternate result]}` and/or `else {[final result]}`. All result expressions must end with a line that returns void (or a semicolon), unless they end with a conditionless else block, in which case it is permitted that all results evaluate to the same non-void type, causing the conditional expression as a whole to take on the value of whichever result is used.
#### Example
```
let result = if 1 > 2 {
    println("Something is wrong...");
    true
} else {
    println("This is fine.");
    false
};
```

### For loops
#### Overview
For loops use the `iter` type described earlier, and their syntax is as follows: `for [variable] in [iter] {[body]}`. The body expression must return void (or end with a semicolon), and the iterator must have a function named iter which returns an iterator (`iter`, `range`, and `array` are the only types which do this currently).

For loops are also one of the only two contexts where `break` and `continue` statements are valid. `break`, when used, stops the execution of the innermost active for (or while) loop, returning to the code after it, and `continue` skips to the next iteration of the innermost loop (or ends it if it's the last iteration). 
#### Example
```
for i in 0..100 {
    if i % 3 == 0 {
        continue;
    }
    printf("{}", i);
    if i > 10 {
        break;
    }
}
// prints 1, 2, 4, 5, 7, 8, 10, 11, and then breaks.
```


### While loops
#### Overview
While loops' syntax is as follows: `while [condition] {[body]}`. The condition must evaluate to a boolean, and the body (which must evaluate to void) will continue to be run until the condition is false.

While loops also allow for the use of `break` and `continue`, and their function is exactly the same as in for loops.
#### Example
```
let i = 0;
while i < 10000 {
    if i % 3 == 0 {
        continue;
    }
    printf("{}", i);
    if i > 10 {
        break;
    }
    i += 1;
}
// This has the same output as the last example
```


### Functions
For a description of functions, see their entry under types, as they are first-class types and can be used in the same way as any other.

## Other Notes
### The Turbofish
#### Overview
In some cases, functions that are generic can't find the types for their generic variables anywhere in the surroundings. This is where the turbofish operator `::<T,U,V,(...)>` is useful, as it allows the caller to specify the generic variables explicitly. The `null` function is a common use case of this, as it accepts no parameters and so can only find a value for its generic type through the return.
#### Example
```
let a = null::<int>();
// Or equivalently:
let a: maybe<int> = null();
```