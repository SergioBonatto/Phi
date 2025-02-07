---
title: README
description: 'Overview of the Phi Lambda Calculus Interpreter project, including installation, usage, architecture, and contribution guidelines'
---

# Phi - A Lambda Calculus Interpreter

Phi is a lightweight interpreter for pure lambda calculus, implemented in Haskell. It provides a simple yet powerful environment for experimenting with lambda calculus expressions and understanding the fundamentals of functional computation.

## Key Features

- Pure Lambda Calculus Support: Implements β-reduction and variable substitution
- Lazy Evaluation: Uses Haskell's natural lazy evaluation strategy
- Robust Parser: Handles nested expressions and complex lambda terms
- Configurable Output: Detailed execution traces and environment inspection
- Maximum Step Limit: Prevents infinite recursion with configurable step limits

## Installation

### Prerequisites

- Stack build tool

### Quick Install

```sh
git clone https://github.com/sergiobonatto/phi.git
cd phi
stack install
```

## Usage

### Basic Syntax

Examples of lambda calculus expressions and definitions.

### Command Line Interface

Instructions for using the phi interpreter via Stack or directly.

### Example Session

A sample interaction with the interpreter, demonstrating input and output.

## Architecture

Overview of the interpreter's modular compiler pipeline:

1. Lexical Analysis
2. Parsing Pipeline
3. Semantic Analysis
4. Evaluation Engine
5. Environment Management

## Contributing

Guidelines for contributing to the project, including areas of interest for improvement.

## Performance Notes

Brief notes on evaluation strategy and performance considerations.

## License

BSD-3-Clause License.

## Further Reading

Links to resources for learning more about lambda calculus and functional programming.