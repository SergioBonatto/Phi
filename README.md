# Phi - A Lambda Calculus Interpreter

Phi is a lightweight interpreter for pure lambda calculus, implemented in Haskell. It provides a simple yet powerful environment for experimenting with lambda calculus expressions.

## Key Features

- **Pure Lambda Calculus Support**: Implements β-reduction and variable substitution
- **Lazy Evaluation**: Uses Haskell's natural lazy evaluation strategy
- **Robust Parser**: Handles nested expressions and complex lambda terms
- **Comment Support**: Handles both single-line (`--`) and multi-line (`{- -}`) comments
- **Maximum Step Limit**: Prevents infinite recursion with configurable step limits

## Installation

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) build tool

### Building

```sh
# Clone the repository
git clone https://github.com/sergiobonatto/phi.git
cd phi

# Build the project
stack build

# install the project
stack install
```

## Usage

### Basic Syntax

```haskell
-- Single line comments start with --
{- Multi-line comments
   use {- and -} -}

-- Basic boolean operations
let true  = λt.λf.t
let false = λt.λf.f
let neg   = λb. (b false true)

-- List implementation using Scott encoding
let cons = λh.λt.λc.λn. (c h (t c n))
let nil  = λc.λn.n
```

### Command Line Interface

```sh
stack exec phi -- <file> [-s] [-c]

Options:
  -s    Display execution statistics (reduction steps and time)
  -c    Show final environment state
```

# Project Structure

The interpreter follows a modular design with clear separation of concerns:

## 1. Core Components
- [`Expression.hs`](src/Expression.hs): Core lambda calculus expression types and data structures
- [`Environment.hs`](src/Environment.hs): Environment management and variable bindings
- [`Evaluator.hs`](src/Evaluator.hs): Expression evaluation and reduction strategies
- [`Substitution.hs`](src/Substitution.hs): Variable substitution and β-reduction implementation

## 2. Parsing Pipeline
- [`Tokenize.hs`](src/Tokenize.hs): Lexical analysis and token generation
- [`Parser.hs`](src/Parser.hs): Main parsing orchestration and integration
- [`ParseCommon.hs`](src/ParseCommon.hs): Shared parsing utilities and helpers
- [`ParseExpr.hs`](src/ParseExpr.hs): Lambda expression parsing logic
- [`ParseApp.hs`](src/ParseApp.hs): Function application parsing
- [`ParseDefinition.hs`](src/ParseDefinition.hs): Let-binding and definition parsing
- [`ParseError.hs`](src/ParseError.hs): Comprehensive error handling types and messages

## 3. Processing
- [`ProcessCode.hs`](src/ProcessCode.hs): Source code processing and evaluation pipeline
- [`ParseTypes.hs`](src/ParseTypes.hs): Core parsing types and data structures

## Testing

The project includes a test suite covering the main components:

```sh
stack test
```

Tests include:
- Tokenization
- Expression parsing
- Definition processing
- Evaluation logic

## License

BSD-3-Clause License.
