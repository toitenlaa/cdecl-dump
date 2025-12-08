cdecl-dump
==========

Dump C declarations visually on the command line.

## How to use

```
./cdecl-dump "int a"
./cdecl-dump "void f(int a)"
./cdecl-dump "unsigned char *const *arr[20][30]"
./cdecl-dump "int (*const fp[20])(void)"
```

## Building

 * `./build.sh` produces a debug build with additional tracing of the parsing stages
 * `DEBUG=0 ./build.sh` produces an optimised executable

## Bugs

 * The program doesn't do strict validation of the declarator in certain cases.
   Unlike the rules of C, it allows functions to return arrays and arrays to have functions as their elements.
 * Only built-in types are supported. For example, `size_t` or `uint64_t` will not be accepted.

## Screenshot

![alt tag](https://raw.github.com/bbu/cdecl-dump/master/screenshot.png)

## How to understand a dump

Each printed stage mirrors the eventual use of the declared variable – it is a sample sub-expression of the
intended use.

We always start with the bare identifier because it is the lowermost expression one can use. Below it,
we draw one or more boxes of what it represents. Pointers and functions are represented by a single box,
arrays are represented by multiple boxes which are optionally truncated for counts larger than 8. Any consequent
stage applies the next operator in the correct order of precedence – `*` for pointer dereferencing, `[]` for
array subscripting, `()` for grouping or for a function call.

After we have applied all the operators, we have reached the final type of the expression, which is
the sole "specifier-qualifier list".

## How it works

The program uses a hand-written, table-driven lexer and parser. This allows relatively easy modification of the code
and the whole solution is therefore self-contained without any external dependencies apart from the standard library.
