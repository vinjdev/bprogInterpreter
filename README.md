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

