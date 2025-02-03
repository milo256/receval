# Receval

Receval, (from REcursive EVALuator) is an expression based statically typed language.
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

Because of the simple syntax, commas, semicolons, and braces are often
not necessary where they would be in other languages.
Make no mistake, however, indentation and line breaks are not significant in receval and
are only used to make the code easier to read.

Everything in receval is an expression, and an expression is
either a function call or an assignment.

Assignments are written with the `=` operator, followed by the name
to assign to, followed by the value to assign.
Names may include any character except whitespace or the following symbols:
`=`, `(`, `)`, `[`, `]`, `{`, `}`, `->`, `/*`, `*/`

```
/* comments are written like this and may span multiple lines */

/* creating variables by assigning the values 10 and 100 to
 * the names x and y */

= x 10

= y 100
```
As receval is statically typed, variables have a fixed type which is
set at the first assignment, infered from the value assigned.

Currently implemented types are `int`, and `function`

The second type of expression is a function call.
Like in many other languages, a function call is the name of the function
followed by a list of arguments enclosed in parenthesis.
Arguments are space separated, not comma separated.

```
/* calling the function `+`, with arguments x and 5 */

+(x 5)
```
Declaring functions is similar to declaring variables

following `->` is the return type, and
inside of the parentesis is the list of arguments.
Since there is no initial value to infer the type from, 
function arguments need to be annotated with their type.

Following the list of arguments is the function code itself,
which must be a single expression.

```
= calculate_area function -> int (int width int height)
    *(width height)
```

Receval is not parsed line-by-line.
The entire file is parsed at once, and then run.
This means global variables and functions may be used "before" they are declared.
It also means that functions can only be called from inside of other
functions. Execution starts in the main function

```
/* yes, this works */

= main function -> int ()
    print_x()

= print_x function -> int ()
    print(x)

= x 10
```

## Built-in functions

Many basic features of receval are implimented as built-in functions.
For example, the simple act of running several functions one after the other
is a builtin function, called `seq()`.
Seq simply runs all of its arguments sequentially and then returns the last one.

Seq is so useful that curly braces (`{}`) are made shorthand for it.

```
/* each function may only have one expression as its body.
 * thankfully, seq is considered one expression */

= main function -> int () {
    = x 5 /* local variable! declared within the function,
           * valid only within the function */
    = y +(x 10)
    print(y)
}
```

`while()` and `if()` can be used for control flow, much like other languages.
While will call its second argument repeatedly until its first returns zero,
and returns the last value returned by its second argument.
If will call its second argument if its first argument is not zero. If provided
with a second argument, it will call that when the first argument is zero.
(like an else block in other languages).
If returns zero if the first argument is zero and there is no second argument.

```

/* assignments are expressions, and they
 * return the value assigned. this code will
 * subtract 1 from n and print n repeatedly,
 * until n is zero */

= count_down function->int(int n)
    while(= n -(n 1) print(n))

= max function->int(int a int b)
    if(>(a b) a b)
```

Lastly, we have `print()` and the arithmentic operators `+()`, `-()`, `*()`, and `/()`.
Print prints its argument, an int, on a new line.

The arithmetic operators perform their respective operations on two ints.
