# Phi - A Pure Lambda Calculus Interpreter

Phi is a sophisticated interpreter for pure lambda calculus, implemented in Haskell. It provides an elegant and powerful environment for exploring lambda calculus expressions, making it ideal for educational purposes and theoretical computer science research.

## Key Features

- **Pure Lambda Calculus Implementation**
  - Full β-reduction support
  - Capture-avoiding variable substitution
  - Configurable evaluation strategies
  - Rich expression syntax

- **Advanced Evaluation**
  - Leverages Haskell's lazy evaluation
  - Configurable step limit for safe recursion
  - Detailed evaluation tracing
  - Expression memoization for performance

- **Robust Parsing System**
  - Comprehensive error handling
  - Support for complex nested expressions
  - Multi-line expression support
  - Rich comment system (`--` and `{- -}`)

- **Development Features**
  - Detailed debugging options
  - Performance statistics
  - Environment inspection
  - Step-by-step evaluation tracing

## Installation

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) build tool (version 2.7.0 or later)
- GHC 9.8.4 or compatible version

### Building from Source

```sh
# Clone the repository
git clone https://github.com/sergiobonatto/phi.git
cd phi

# Build the project
stack build

# Run tests (recommended)
stack test

# Install globally
stack install
```

## Usage Guide

### Expression Syntax

```haskell
-- Define basic boolean operations
let true  = λt.λf.t          -- Returns first argument
let false = λt.λf.f          -- Returns second argument
let and   = λp.λq. p q false -- Logical AND
let or    = λp.λq. p true q  -- Logical OR
let not   = λb. b false true -- Logical NOT

-- Church numerals
let zero  = λf.λx.x
let succ  = λn.λf.λx.f (n f x)
let add   = λm.λn.λf.λx.m f (n f x)

-- List operations (Scott encoding)
let nil   = λc.λn.n
let cons  = λh.λt.λc.λn. c h (t c n)
let head  = λl. l (λh.λt.h) undefined
let tail  = λl. l (λh.λt.t) nil
```

### Advanced Command Line Interface

```sh
stack exec phi -- [options] <input-file>

Options:
  -s    Show execution statistics (steps, time)
  -c    Display final environment state
  -d    Enable debug mode
  -t    Show evaluation trace
  -m N  Set maximum evaluation steps (default: 1000)
```

### Example Session

```sh
$ cat examples/church.phi
let zero = λf.λx.x
let succ = λn.λf.λx.f (n f x)
let two = succ (succ zero)

(two)

$ stack exec phi -- examples/church.phi -s
=> λf.λx.f (f x)
==================================================
Execution time: 0.000123 seconds
Number of reduction steps: 4
```

Coletando informações do workspaceHere's an improved version of the Project Architecture section:

## Project Architecture

### Core Components
- `Expression.hs`: Core abstract syntax tree (AST) implementation with:
  - Smart constructors for expressions
  - Pattern matching facilities
  - Pretty printing functionality
- `Environment.hs`: Environment management with:
  - Variable binding
  - Scope handling
  - Context tracking
- `Evaluator.hs`: Expression evaluation featuring:
  - Multiple reduction strategies
  - Performance optimization
  - Step-by-step execution
- `Substitution.hs`: Variable substitution with:
  - Capture avoidance
  - Name freshening
  - Alpha conversion

### Parser Pipeline
- `Tokenize.hs`: Lexical analysis providing:
  - Token classification
  - Comment handling
  - Error recovery
- `Parser.hs`: Expression parsing with:
  - Combinator-based approach
  - Precedence handling
  - Error reporting
- `Common.hs`: Shared utilities for:
  - Token handling
  - Error management
  - Parser primitives
- `App.hs`: Application parsing with:
  - Left associativity
  - Nested expression support
  - Precedence rules

### Processing Layer
- `ProcessCode.hs`: Code processing pipeline:
  - Input preprocessing
  - Definition handling
  - Error propagation
- `ProcessLine.hs`: Line processing with:
  - Statement parsing
  - Context management
  - Interactive evaluation
- `StripLine.hs`: Source preprocessing:
  - Whitespace normalization
  - Comment removal
  - Line continuation

Each component is designed to be modular and composable, following functional programming principles and providing clear separation of concerns.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new features
4. Submit a pull request

### Development Guidelines

- Maintain pure functional style
- Add unit tests for new features
- Update documentation
- Follow existing code formatting
- Use meaningful commit messages

## Testing

Comprehensive test suite covering:

```sh
stack test

# Run specific test groups
stack test --ta '-p "Parser"'
stack test --ta '-p "Evaluator"'
```

- Unit tests for core components
- Integration tests for parser
- Property-based tests for evaluation
- Performance benchmarks

## License

BSD-3-Clause License

## Acknowledgments

- Inspired by the lambda calculus works of Alonzo Church
- Built with modern Haskell practices
- Community contributions welcome
