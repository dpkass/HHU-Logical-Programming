:- use_module(library(plunit)).

% ------------------ Task 2 ------------------
% Simplify the following two propositional formulae by rewriting implications to disjunctions and
% moving negations inwards to the literals.

% 1. (a ⇒ (b ∧ ¬a)) ⇒ b
% ¬(¬a ∨ (b ∧ ¬a)) ∨ b
% (a ∧ (¬b ∨ a)) ∨ b

% 2. (¬(a ∨ b)) ∧ (¬b ⇒ a)
% ¬(a ∨ b) ∧ (b ∨ ¬a)
% ¬a ∧ ¬b ∧ (b ∨ ¬a)



% ------------------ Task 3 ------------------
% Represent the automata shown as facts delta(-In, -Literal, -Out) in Prolog. For instance, delta
% (1, d, 2). Implement a predicate accept(+L) which receives a list of atoms as argument and is
% true if the automata accepts the word represented by the list.
d(1, d, 2).
d(2, a, 2).
d(2, b, 2).
d(2, c, 3).
d(2, d, 4).
d(2, e, 5).
d(3, d, 6).
d(6, c, 5).

f(4).
f(5).

accept(L) :- accept(L, 1).

accept([], S) :- f(S).
accept([Eps|L], S) :- d(S, Eps, T), accept(L, T).


% task 4


:- begin_tests(automaton).

test(accept,[nondet]) :-
    accept([d,a,b,a,b,b,b,c,d,c]).

test(accept2,[nondet]) :-
    accept([d,a,b,d]).

test(accept3,[nondet]) :-
    accept([d,e]).

test(accept4,[nondet]) :-
    accept([d,a,b,a,b,a,a,a,a,b,b,a,b,b,a,a,b,b,b,e]).

test(not_accept,[fail]) :-
    accept([d,a,b,a,e,d,c]).

test(not_accept2,[fail]) :-
    accept([d,a,b,d,d]).

test(not_accept3,[fail]) :-
    accept([d,a,b,a,b,a,a,a,b,b,a,a,b,b,c,b,e]).

:- end_tests(automaton).

:- begin_tests(sat).

test(sat, [nondet]) :-
    is_true(cst(true)).

test(sat2, [nondet]) :-
    is_true(not(cst(false))).

test(sat3, [nondet,true(A == false)]) :-
    is_true(not(cst(A))).

test(sat4, [nondet,true((A == true, B == false))]) :-
    is_true(and(cst(A), not(cst(B)))).

test(sat5_all, [nondet,all(A = [true])]) :-
    is_true(or(cst(A), not(cst(true)))).

test(sat6, [nondet]) :-
    is_true(or(not(and(cst(true),cst(false))),cst(false))).

test(sat7,[nondet]) :-
    is_true(or(not(and(cst(true),cst(false))),and(cst(true),or(cst(false),cst(true))))).

test(sat8, [nondet,true((A == false; B == true))]) :-
    is_true(or(not(and(cst(true),cst(A))),cst(B))).

test(sat9, [nondet,true((A == false, B == true))]) :-
    is_true(and(not(or(cst(false),cst(A))),cst(B))).

test(sat10_all, [nondet,all(B == [true])]) :-
    is_true(or(not(or(cst(false),cst(true))),cst(B))).

test(unsat,[fail]) :-
    is_true(not(cst(true))).

test(unsat2,[fail]) :-
    is_true(or(not(cst(true)),cst(false))).

test(unsat3,[fail]) :-
    is_true(and(not(cst(true)),or(cst(false),cst(true)))).

test(unsat4,[fail]) :-
    is_true(not(or(not(cst(true)),cst(true)))).

:- end_tests(sat).
