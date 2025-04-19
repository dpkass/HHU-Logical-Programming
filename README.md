# Logic Programming in Prolog

**Author:** Taha El Amine Kassabi  
**Course:** Introduction to Logic Programming (WS 2023/24)  
**Instructor:** Prof. Dr. Michael Leuschel  
**University:** Heinrich Heine University Düsseldorf (HHU)

---

## 📚 Overview

This repository contains my solutions to **12 graded exercise sheets** and a **final SAT‑solver project** for the HHU course *Introduction to Logic
Programming*.  
Assignments span propositional logic, unification, SLD‑resolution, search algorithms, DCGs and CLP(FD).  
The project implements a full **DPLL SAT solver** in SWI‑Prolog.

---

## 📂 Repository Structure

```
Exercise Sheets/
└── Exercise 1 … 12/          # PDF task sheet, *.pl solution, optional images / docs
    └── (e.g. Exercise 10 → Alpha‑Beta pruning & number‑scrabble bot)

Project/
├── sat_solver.pl             # DPLL solver implementation
├── dimacs_parser.pl          # Helper to read DIMACS CNF
├── sat_benchmarks.pl         # Test harness
├── sat_benchmarks/*.cnf      # CNF benchmarks (1.cnf … large_formula_example.cnf)
└── project.pdf               # Project instructions
```

---

## 🧠 Topics Covered

| Sheet | Core Topics & Skills                                                            |
|-------|---------------------------------------------------------------------------------|
| 01    | Prolog first steps, family facts, recursion                                     |
| 02    | Knowledge bases, FOL translation, list utilities                                |
| 03    | Resolution & DPLL basics, automata, SAT evaluation predicate                    |
| 04    | Proof by contradiction, list compression, binary‑tree traversals, GCD & coprime |
| 05    | CNF conversion, formula simplification, Euler φ, Horn/denial recognition        |
| 06    | Unification algorithm, permutations, drop‑nth                                   |
| 07    | CNF & resolution in FOL, logical proofs                                         |
| 08    | SLD trees, iterative deepening (15‑Puzzle)                                      |
| 09    | Higher‑order predicates, maplist/include, A* search                             |
| 10    | Minimax & Alpha‑Beta pruning, number‑scrabble bot                               |
| 11    | Proof strategies, advanced logic topics                                         |
| 12    | Difference lists, DCGs (balanced brackets, Sudoku parser), CLP(FD) intro        |

---

## 🚀 SAT‑Solver Project

*Transforms arbitrary propositional formulas into CNF and solves them via **DPLL** with unit‑propagation & backtracking.*

```prolog
?- [sat_solver].
?- to_cnf(implies(lit(A), or(lit(B), not(lit(A)))), CNF).
CNF = [[not(A), B], [not(A)]].
?- solve(CNF).
A = false ;
B = true  ;
false.
```

Run full benchmarks:

```prolog
?- run_benchmarks.
```

---

## 💾 Setup

Install SWI‑Prolog (≥ 8.2)

```bash
# linux
sudo apt install swi-prolog 
```

```bash
# macOS
brew install swi-prolog
```

---

## 🚀 Usage

```prolog
swipl
?- ["Exercise Sheets/Exercise 4/exercise4_solution.pl"].
?- run_tests.    % when unit tests are provided
```

Project usage:

```prolog
?- ["Project/sat_solver.pl"].
?- solve([[not(A), B], [A, not(B)]]).
```

---

## 📝 Notes

* Solutions are written for **SWI‑Prolog** and follow idiomatic declarative style.
* Each sheet folder contains both *task* & *solution* files for easy comparison.
* Benchmarks demonstrate solver correctness; runtime optimisation was not graded.
