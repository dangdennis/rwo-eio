# Concurrent Programming with Eio

Below are [Eio](https://github.com/ocaml-multicore/eio) translations of [the code examples](https://github.com/realworldocaml/examples) in [Real World OCaml - Chapter 16. Concurrent Programming with Async](https://dev.realworldocaml.org/concurrent-programming.html). The section titles follow those in the book for easy cross-reference.

See `dune-project` for dependencies.

Commands to run project
```bash
make dev # builds the ocaml project in watch mode, aliases `dune build -w`
make run # executes main.ml that runs all the examples, aliases `dune exec main`
dune exec server -- -p 8080 -uppercase false
dune exec search ocaml functionalprogramming monad
```

See `lib/rwo_eio.ml` for all examples.
See `bin/main.ml` to see how examples are used.