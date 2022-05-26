# Programming Languages and Translators project

- Jayshil Dave - jyd2111
- Sounak Ray - sr3846
- Chinmay Garg - cg3286
- Umang Raj - ur2136
- Jesse Cohen - jec2268

## Links

https://drive.google.com/drive/folders/1f3UkBnF5sGwEqPNdlVrqoEDswxd_iXiQ?usp=sharing

## Data types Supported

- Int
- String
- Bool
- Node
- Edge
- Graph
- NodeList
- EdgeList

## Testing

- Parser test - test1.ml
- Semantic test - test2.ml

Follow example.grf for running testcases.

## Commands to run

- ocamlbuild test1.native
- ocamlbuild test2.native
- ocamlyacc -v graphcparse.mly for checking shift/reduce and reduce/reduce conflicts

### Build the G-Raph-C compiler

```
make all
```

### Run the G-Raph-C compiler and generate llvm code

```
./graphc.native -a ./tests/[filename].grf  // AST generation
./graphc.native -s ./tests/[filename].grf  // Semant generation
./graphc.native -l ./tests/[filename].grf  // IR Code generation
```

### Run the single file

```
bash ./run.sh [filename]
```
where filename = ./tests/[filename].grf


### Run the all tests file

```
bash ./testallfiles.sh
```

### Compiler files

- `ast.ml`: abstract syntax tree (AST) definition
- `scanner.mll`: scanner
- `graphcparse.mly`: parser
- `sast.ml`: definition of the semantically-checked AST
- `semant.ml`: semantic checking
- `irgen.ml`: LLVM IR code generator

### Other files

- `test1.ml`: the file to test the scanner and parser
- `test2.ml`: the file to test the semantic checker
- `graphc.ml`: top-level file to test and run G-Raph-C compiler
