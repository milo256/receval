# Receval

Receval, (from REcursive EVALuator) is a simple, statically typed interpreted
programming language i'm currently developing.
I created receval to learn about programming languages.
Its goals are to be simple, fast, and useful enough to write real programs with.

## Usage

Receval is written in c and depends on only the standard library.
To build, clone this project and run `make` in the project directory.

To run a receval (.re) file, just run the receval command with the
path to file as the first argument.

```./receval examples/fib.re```

## Syntax Overview

Receval has an extremely simple expression-based syntax.

Because of the simple syntax, commas, semicolons, and braces are often not
necessary where they might be in other languages.
whitespace is not significant.

### Comments
```
-- this is a single line comment

-! this is
 a comment
 spanning
 multiple
 lines !-
```

### Assignments

Assignments are written with the `=` operator, followed by the name
to assign to, followed by the value to assign.

```
-! creating variables by assigning the
 ! values 10 and 100 to the names x and y
 !-

= x 10

= y 100
```
As receval is statically typed, variables have a fixed type which is
set at the first assignment, infered from the value assigned.
Variables are function-scoped.

### Function Declarations

Declaring functions is similar to declaring variables:
```
= calculate_area function -> int (int width int height)
    *(width height)
```
Following `->` is the return type, and inside of the parentesis is the list of
parameters.
Since there is no initial value to infer the type from, function arguments
need to be annotated with their type.
Following the list of arguments is the function code itself, which must be a
single expression.

To create an expression from multiple expressions, use braces.
Braces (curly brackets) evaluate each expression inside them in order,
returning the value of the last one.
```
= x 10
= print_and_add_to_x function -> int (int addend) {
    print(addend)
    = x +(x addend) 
}
```

### Entry Point

Arbitrary code is only allowed within functions.
Receval looks for a function named `main` as the entry point to the program.

```
= main function -> void () {
    = hello "hello world"
    print(hello)
}
```


### Function Calls

Like in many other languages, a function call is written with the name of the
function first, followed by a list of parameters enclosed in parenthesis.
Parameters are space separated.

```
    = x 5
    
    -! calling the function `+`, with parameters x, 5, and
     ! the result of a call to `-(2 3)`` !-

    +(x 5 -(2 3))
```


### Types

Currently implimented types are `void`, `int`, `str`, and `function`
```
= main function -> void () {
    -- void
    = v {}

    -- int
    = i 1234

    -- str
    = s "hello"

    -- function
    = f function -> void () print("hello world")
}
```



### Control Flow

Receval has `if`/`else` and `while`.
These work the same way they do in other languages.
Conditions must be integers, nonzero values are "truthy".
`if`/`else` returns the result of the branch that evaluated, so it can be used
like a ternary operator.
While and if without else just return void.

```
= main function -> void () {
    = true 1
    = size if true "big" else "small"
    print(size)
}
```
```
= print_countdown function -> void () {
    = counter 10
    while -(counter 1) print(counter)
}
```

### Built-in functions

- `+()`
- `-()`
- `*()`
- `/()`
- `print()`

