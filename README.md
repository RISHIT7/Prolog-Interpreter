# Prolog-Interpreter
A Mini Prolog interpreter written in OCaml

A README file serves as an introductory document for your project, providing essential information to users, contributors, and other stakeholders. Here's a template for a README.md file for a Prolog interpreter project:

## Table of Contents
- [Overview](#overview)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Examples](#examples)
- [License](#license)

## Overview
Prolog is a logic programming language that excels in applications involving symbolic reasoning and complex data relationships. This project aims to bring the power of Prolog to developers and researchers through an intuitive interpreter.

## Features
- **Standard Prolog Syntax**: Supports most of the common Prolog syntax and predicates.
- **Querying and Reasoning**: Perform queries and logical reasoning in a user-friendly REPL (Read-Eval-Print Loop).
- **Built-in Predicates**: A selection of built-in predicates to facilitate coding in Prolog.
- **Error Handling**: Provides helpful error messages and guidance when syntax or semantic issues arise.
- **Extensibility**: Easily extend the interpreter with custom predicates and features.

## Installation
To install the Prolog interpreter, follow these steps:

1. **Clone the repository**:
   ```bash
   git clone https://github.com/RISHIT7/Prolog-Interpreter.git
   ```

2. **Run the interpreter**:
   ```bash
   make
   ```
   Will run the default prolog file Test.pl \
   To run a custom file, write the prolog file into the Test folder, assuming the name is prolog.pl
   ```
   make test_file=Test/prolog.pl
   ```

## Usage
After running the interpreter, you can start typing Prolog queries directly into the REPL. The interpreter will evaluate your queries and provide results.

Example queries:
- To find a solution to a query:
  ```prolog
  ?- member(X, [1, 2, 3]).
  ```

- To assert a fact or rule:
  ```prolog
  ?- hastype(gt(intT(3), intT(4)), X).
  ```

- Special identifiers:
  ```prolog
  ?- integer(3).
  ```

## Examples
You can find example Prolog scripts in the `Test` directory. These scripts demonstrate how to use the interpreter for various tasks.

## License
This project is licensed under the [MIT License](LICENSE). Please see the LICENSE file for more details.
