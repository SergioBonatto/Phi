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
let compose = λf. λg. λx. f (g x)
let id = λx. x
compose id id 42

$ phi-exe example.phi -s
=> 42
------------------
Steps: 4
Time: 0.000123s
```

## Architecture

The interpreter follows a classic compiler pipeline:

1. **Lexical Analysis** ([Tokenize.hs](src/Tokenize.hs))
   - Converts raw text into tokens
   - Handles lambda syntax (λ), parentheses, and identifiers

2. **Parsing** ([Parser.hs](src/Parser.hs))
   - Implements recursive descent parsing
   - Builds AST with proper precedence rules
   - Handles let-bindings and expressions

3. **Evaluation** ([Evaluator.hs](src/Evaluator.hs))
   - Performs β-reduction
   - Manages variable substitution
   - Implements call-by-name evaluation

4. **Environment Management** ([Environment.hs](src/Environment.hs))
   - Tracks variable bindings
   - Handles lexical scoping

## Contributing

Contributions are welcome! Areas of particular interest:

- Adding alpha-conversion to handle name conflicts
- Implementing Church encoding for basic data types
- Adding type inference
- Improving performance for complex reductions

Please see CONTRIBUTING.md for guidelines.

## Performance Notes

- The interpreter uses lazy evaluation by default
- Maximum reduction steps can be configured to prevent infinite loops
- Complex expressions may require increasing the step limit with `-s` flag

## License

BSD-3-Clause License. See

LICENSE

 for details.

## Further Reading

- [Introduction to Lambda Calculus](https://www.cs.cornell.edu/courses/cs3110/2008fa/lecturenotes/L02-lambda-calculus.pdf)
- [Implementing a Lambda Calculus Evaluator in Haskell](https://stackoverflow.com/questions/tagged/lambda-calculus+haskell)
- [The Implementation of Functional Programming Languages](https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/)
