# BC in OCaml

This is a bc evaluator in OCaml for the Programming Language Concepts class.

## Notes to Grader

This implementation creates a new scope for both the arguments and body of each function created. That scope is destroyed on completion of the function. Scopes are created for no other reason, aside from the initial global scope.

Due to time constraints, I opted to start with a lot of full "integration" tests that use multiple "features" taken from the ANTLR implementation of bc, to test easily test the coverage of the code. Time ran out before I could finish doing more unit tests (which take more time to create for the same amount of coverage), but I feel the existing tests seem to suffice.

Also, I did change the post action part of for loops to be an expression rather than a statement, as it seemed more inline with [bc's description](https://www.gnu.org/software/bc/manual/html_mono/bc.html#SEC15) (which calls for all three top parts to be expressions).

## Usage

### Installing

This project requires:
- OCaml
- Opam
- Dune
	- `opam install dune`
- Base and Core library
	- `opam install core`

### Running

To run the unit and integration test from the terminal:

```bash
dune runtest
```

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
