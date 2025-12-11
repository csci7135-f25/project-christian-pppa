# project/code

This folder contains the source code for my course project, and all of the necessary instructions to run it. 

### Organization

The folder `mira` contains a .NET solution with 3 projects. 

`ext` contains external source code needed to run the project (In this case, the source code of the P language's parser/type checker. See https://github.com/p-org/P for full source).

`mira-test` contains a lightweight test setup for Mira, which shows how to actually run it (there is no front-end, its just a library). `test-programs` includes some example programs used by the test library.

`mira` contains an F# library which comprises the implementation. It is primarily divided into 4 modules: 

- `CFG.fs` defines a translation from P source programs to a control-flow automaton representation used by the analysis.

- `NumericDomain.fs` defines an abstract domain of intervals with symbolic arithmetic, as well as abstract operators needed in the fragment of P under consideration. More operations can of course be added, but these are sufficient for the problemset. 

- `Domain.fs` defines the abstract domain used by the analysis. This includes the abstraction of stores, location, messages, and the automaton. Join and widen are defined here.

- `AbstractSemantics` defines the transition system of Mira, alongside a worklist algorithm to actually run the analysis starting from an initial state. This is the bulk of the implementation, as it includes abstract evaluation, and the abstract transition system, alongside helper functions for interacting with the message automaton.

### Running Mira

`dotnet test` in the `mira` folder (containing the `.sln` file) will compile and run tests (may take some time the first run, due to installing a runtime for F# and C#(the parser)). Currently one fails, as the program tested is indeed unsafe. To get more debug information on tests, it may be desirable to use a debugger. Some information on states seen, and automata produced, is printed currently.