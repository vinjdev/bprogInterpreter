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

## Supported data types
- Numbo  -> Integer
- Deci   -> Float
- Truthy -> Bool
- Wordsy -> String
- Bag    -> List
- Block  -> Code Block
- Tag    -> Symbol

This is just an abstraction for the data types
Which is only viewable for debuggingg with `:s` and `:m`

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
## Debugging
```bash
# Stack state
:s
>>> Stack: [Numbo 2]

# Dictionary state
:m
>>> Map: fromList [("age",Numbo 20)]

# Deleting dictionary state
:D
```
# Importing prelude functions
```bash
prelude
```
see imported functions with `:m`

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
>>> [2,3]

[ ] empty
>>> True

[ 1 2 3 ] length
>>> 3

0 [ 1 2 3 ] cons
>>> [0,1,2,3]

[ 1 2 ] [ 3 4 ] append
>>> [1,2,3,4]

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

## Functions
```bash
value 20 := 
value
>>> 20

counter 3 := counter times { " hello " print }
>>> "hello"
    "hello"
    "hello" # will print 4 times, as repl will always print the top value
```

## Test
```bash
stack test
```
will show 104 examples of valid user input

## Program structure
```bash
src
├── Bprog             # types, errors, repl mode and normal mode
│   ├── Errors.hs
│   ├── NormalMode.hs
│   ├── Parser.hs
│   ├── ReplMode.hs
│   └── Types.hs
├── Interpreter        # interpreter logic
│   ├── Arithmetics.hs
│   ├── BprogIO.hs
│   ├── Dictionary.hs
│   ├── Interpreter.hs
│   ├── ListOp.hs
│   ├── ParseOp.hs
│   └── StackOp.hs
└── prelude
    └── prelude.bprog
```


