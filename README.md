# Boolean CNF Solver

A simple OCaml program that finds satisfying assignments for boolean expressions in Conjunctive Normal Form (CNF).

## Description

This program takes boolean expressions in CNF format and attempts to find a satisfying assignment of variables. It supports:
- Boolean variables (single lowercase or uppercase letters a-z A-Z)
- NOT operator
- AND operator between clauses
- TRUE and FALSE literals

## Building

The project uses dune as its build system. To build:

```bash
dune build
```

## Running

After building, you can run the program with:

```bash
dune exec BooleanCNF
```

## Usage

Enter boolean expressions using spaces between tokens. For example:

```
a AND NOT B
```

This will attempt to find values for variables a, b, and c that satisfy the expression.

## Limitations

1. The program does NOT support parentheses in expressions. All expressions must be in CNF format.
   Example of unsupported syntax: `(a AND b) OR c`

2. Only supports the following operators:
   - NOT (negation)
   - AND (between clauses)

3. Expressions must already be in CNF form - the program does not convert arbitrary boolean expressions to CNF

## Example

Input:
```
a AND NOT b AND C
```

This represents the CNF formula: (a ∧ ¬b ∧ c).

The program will output a satisfying assignment for the variables if one exists, or indicate that the expression is not satisfiable.