# KOAK EPITECH

<p align="center">
    <img 
    width="150"
    height="150"
    src="logos/haskell.svg"
  >
    <img 
    width="150"
    height="150"
    src="logos/llvm.svg"
  >
    <img 
    width="150"
    height="150"
    src="logos/vscode.svg"
  >
</p>

This project aims to implement a compiler/interpreter of the KOAK language, a simplified version of the original Kaleidoscope Language (which can be found [here](https://llvm.org/docs/tutorial/LangImpl01.html)), using haskell language and llvm.
This project is divided in 4 parts:
1. Parsing
2. Building the AST
3. Inferring Types
4. Code Compilation

## Parsing

Once the program reads the files containing the documentation written in KOAK it will identify all of its components, such as: literals(integer, double, string, etc), binary operators (+,-,*,/,etc), variables, functions, calls, etc...

## Building the AST

Once the parsing is done, the parser will build an abstact syntax tree that will map all the components in order.

## Inferring Types

Once the tree is built, inferring will get all the elements from the tree and attribute their types.

## Code Compilation

Once all the previous steps are completed, the tree will be sent to the code generation which will compile it and create a binary using llvm bindings.

# How to build

## Dependencies

You will need to install [Stack](https://docs.haskellstack.org/en/stable/README/), [Clang](https://clang.llvm.org/get_started.html) and [Llvm](https://releases.llvm.org/download.html)

## Build

```
$> git clone https://github.com/Erecutoro/KOAK.git
$> make
$> make tests_run (for unit tests)
```

## Usage

```
$> ./koak files
```
