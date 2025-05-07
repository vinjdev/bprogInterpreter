# BPROG Interpreter

A simple interpreted stack based programming language written in haskell

## Features
- Arithmetic: `1 2 +`
- Stack manipulation: `dup`, `swap`, `pop`
- Lists: `[ 1 2 3 ]`
- Blocks and control flow: `{ ... }`, `if`, `loop`, `times`
- Variables: `x 42 :=`, `x`
- Print and Read: `print`, `read`
- List operations: `head`, `tail`, `empty`, `length`, `cons` `append`, `each`, `map`, `foldl`

## Build and run
build:
```bash
stack build
```
#### Run REPL mode:
```bash
stack run
```
output:
```bash
Welcome to the BPROG REPL
bprog>
```
#### Run Normal mode (.bprog file)
```bash
stack run path/to/script.bprog
```
## IO

`print`
```bash
" hello world " print
>>> "hello world"
```
Note: in REPL mode will always print the last stack
so this would print double

`read`
```bash
read parseInteger hei swap := hei
Input: 20  # <- user input
>>> 20
```
showcases a simple parsing operation

## Control flow
`if`,`loop`,`times`
```bash
True if { " this is a true block " } { " this is a false block " } # NOTE: this prints as its in REPL
>>> "this is a true block"                                         # In normal mode it will need a print statement

False if { 10 print } { 20 print }
>>> 20


```
### Logical Expressions
```bash
True False &&
>>> False

False False ||
>>> False

True not
>>> False

10 not   # does not work with floats
>>> -10
```
## List and list operations
List examples
```bash
[ 1 2 3 ]
[ " hello " " world " ]
[ 1 " hello " 2 " world " [ False True ] hello_symbol ]
```

`head`,`tail`,`length`,`cons`,`append`,`each`,`map`,`foldl`
```bash
[ 1 2 3 ] head
>>> 1

[ 1 2 3 ] tail
>>> [ 2 3 ]

[ ] empty
>>> True

[ 1 2 3 ] length
>>> 3

0 [ 1 2 3 ] cons
>>> [ 0 1 2 3 ]

[ 1 2 ] [ 3 4 ] append
>>> [ 1 2 3 4 ]

[ 1 2 3 ] each { print }
>>> 1
    2
    3

[ 1 2 3 ] map { 10 * }
>>> [ 10 20 30 ]

[ 1 2 3 ] 0 foldl { + }
>>> 6
```
All these function will work on strings and code block
EXCEPTION: `each`,`map`,`foldl`

## 


