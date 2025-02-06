# Phi - A Lambda Calculus Interpreter

Phi is a lightweight interpreter for pure lambda calculus, implemented in Haskell. It provides a simple yet powerful environment for experimenting with lambda calculus expressions and understanding the fundamentals of functional computation.

## Key Features

- **Pure Lambda Calculus Support**: Implements β-reduction and variable substitution
- **Lazy Evaluation**: Uses Haskell's natural lazy evaluation strategy
- **Robust Parser**: Handles nested expressions and complex lambda terms
- **Configurable Output**: Detailed execution traces and environment inspection
- **Maximum Step Limit**: Prevents infinite recursion with configurable step limits

## Installation

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) build tool

### Quick Install

```sh
# Clone the repository
git clone https://github.com/sergiobonatto/phi.git

# Enter the directory
cd phi

# Build and install
stack install
```

## Usage

### Basic Syntax

```haskell
# Identity function
let id = λx. x

# Function application
let apply = λf. λx. f x

# Church numerals
let zero = λf. λx. x
let succ = λn. λf. λx. f (n f x)
```

### Command Line Interface

There are two ways to use the phi interpreter:

1. Using Stack directly:
The basic method that works immediately after installation.
```sh
stack exec phi -- <file> [-s] [-c]
```
2. Using the phi command:
If you've added `~/.local/bin` to your PATH, you can use the simpler command format.

```sh
phi <file> [-s] [-c]
```
Options available for both methods:
  `-s`    Display execution statistics (reduction steps and time)
  `-c`    Show final environment state

```sh
phi [OPTIONS] <input-file>

Options:
  -s    Display execution statistics (reduction steps and time)
  -c    Show final environment state
```

### Example Session

```sh
$ cat > example.phi
let true  = λt.λf.t
let false = λt.λf.f
let neg   = λb. (b false true)

let cons = λh.λt.λc.λn. (c h (t c n))
let nil  =       λc.λn.n


(neg true)

$ phi-exe example.phi -s
=> λt. λf. f
------------------
Steps: 6
Time: 0.000695s
```

## Architecture

The interpreter follows a modular compiler pipeline with clear separation of concerns:

1. **Lexical Analysis** ([`Tokenize.hs`](src/Tokenize.hs))
   - Converts raw text into token streams
   - Handles lambda syntax (λ), parentheses, identifiers
   - Filters out whitespace and comments

2. **Parsing Pipeline**
   - [`Parser.hs`](src/Parser.hs): Main parser orchestrator
   - [`ParseCommon.hs`](src/ParseCommon.hs): Core parsing primitives
   - [`ParseExpr.hs`](src/ParseExpr.hs): Expression parsing
   - [`ParseApp.hs`](src/ParseApp.hs): Function application parsing
   - [`ParseDefinition.hs`](src/ParseDefinition.hs): Let-binding parsing
   - [`ParseTypes.hs`](src/ParseTypes.hs): Parser type definitions
   - [`ParseError.hs`](src/ParseError.hs): Error handling types

3. **Semantic Analysis**
   - [`Expression.hs`](src/Expression.hs): AST data structures
   - [`ProcessCode.hs`](src/ProcessCode.hs): Code processing and validation

4. **Evaluation Engine**
   - [`Evaluator.hs`](src/Evaluator.hs): Core evaluation logic
   - [`Substitution.hs`](src/Substitution.hs): Variable substitution
   - Implements call-by-name evaluation strategy
   - Handles β-reduction with configurable step limits

5. **Environment Management** ([`Environment.hs`](src/Environment.hs))
   - Manages variable bindings and scope
   - Provides symbol table functionality
   - Tracks definition dependencies

The modular design allows for easy extension and modification of individual components while maintaining clean interfaces between stages.

## Contributing

Contributions are welcome! Areas of particular interest:

- Adding alpha-conversion to handle name conflicts
- Implementing Church encoding for basic data types
- Adding type inference
- Improving performance for complex reductions

## Performance Notes

- The interpreter uses lazy evaluation by default
- Maximum reduction steps can be configured to prevent infinite loops
- Complex expressions may require increasing the step limit with `-s` flag

## License

BSD-3-Clause License.

## Further Reading


- [Introduction to Lambda Calculus](https://www.cs.cornell.edu/courses/cs3110/2014sp/recitations/25/lambda-calculus.html)
- [Implementing a Lambda Calculus Evaluator in Haskell](https://stackoverflow.com/questions/tagged/lambda-calculus+haskell)
- [The Implementation of Functional Programming Languages](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf)
