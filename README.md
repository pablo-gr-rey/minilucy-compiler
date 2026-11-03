# Compiler from a subset to Lustre (a synchronous stream language) to C, implemented in OCaml.

## Usage

I used OCaml 4.14.2 for compatibility reasons (to be able to run Lustre), but more recent compilers should also work.
To build, simply run `dune build`.
Then, to generate C files in the folder `c_output` for the example `examples/test1.lus` along with a `main.c` file to simulate the node `mod_count`, run
```
dune exec src/main.exe -- -v examples/test.lus mod_count -gen-main -output-folder c_output
```

Then, `cd c_output`, compile with e.g. `gcc -O2 test.c main.c -o main` and run `./main` to simulate the node.
Hopefully, the result should be the same (formatting aside) as `lv6 examples/test.lus -exec -n mod_count`.

## Description of the project

I mostly followed the structure from [1]. For now, the compilation is pretty naive, introducing many temporary variables and featuring only few optimizations, but more should be implemented.

Compilation passes:
- Parsing (`parse_ast.ml`, `lexer.mll`, `parser.mly` and some util modules)
- Typing fonctions (`typed_ast.ml`, `typed_ast_utils.ml`, `typing.ml`) (in the usual sense: infering types, checking correctness and giving the type int, bool or real to every subexpression)
- Clock typing and inference and normalization to a smaller, simpler language: `norm_ast.ml` and `norm.ml`. For now, there is no clock calculus; since we assume that all input and output of functions run on the same base clock, clock inference can be conducted without much complexity
- Simplification of the dataflow in `simpl_dataflow.ml` (for now, the only operation is removal of useless introduced temporary variable by merging variables related by equations `y = x`, created quite often by the normalization process)
- Scheduling equations, taking into account data dependencies and fbys in `schedule_full.ml`, duplicating variables when necessary (for example, to be able to schedule `x = 0 fby y; y = 0 fby x` we have to introduce a temporary variable `x = cx; cx = 0 fby y; y = 0 fby x`)
- Conversion to Obj-C in `objc_ast.ml` and `objc.ml`, a simple object-oriented imperative language (which involves building the memory structure for each node, as well as step and reset methods)
- Conversion to C in `c_printer.ml` (for now, simply produce a header and a source file containing for each node reset and step functions, as well as the associated memory structures). TBD: an option allowing to simulate the nodes, adding a main.c file

I plan to implement several features and optimizations:
- initialization analysis (for now, the compiler does not check that nil value are not used, e.g. by a simple node `y = pre x`)
- reduce the number of temporary variables, introduced in the normalization and scheduling compilation parts
- implement common subexpression elimination
- introduce heuristics for inlining (or at least support an `inline` directive)
- implement clock calculus to fully support Lustre v6 clock system

## Notes

I have yet to carefully review the code and thoroughly test it. Thus, it is possible (and in fact very likely) that it contains some bugs.

Except from a part of the typing and parsing modules (which were given in class), all the code is my own. No LLM was used for this project.

-- Pablo Rey

## Bibliography

[1] Biernacki, D., Colaço, J. L., Hamon, G., & Pouzet, M. (2008, June). Clock-directed modular code generation for synchronous data-flow languages. In Proceedings of the 2008 ACM SIGPLAN-SIGBED conference on Languages, compilers, and tools for embedded systems (pp. 121-130).
