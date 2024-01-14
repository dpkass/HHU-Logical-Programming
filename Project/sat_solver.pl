:- module(sat_solver,[normalise/2, to_cnf/2, solve/1]).

:- load_test_files([]).

%% normalise(+Formula, -NFormula).
normalise(lit(A), lit(A)).

normalise(equivalence(A, B), and(P, Q)) :-
  normalise(implies(A, B), P),
  normalise(implies(B, A), Q).
normalise(implies(A, B), or(NA, NB)) :-
  normalise(not(A), NA),
  normalise(B, NB).

normalise(and(A, B), and(NA, NB)) :-
  normalise(A, NA),
  normalise(B, NB).
normalise(or(A, B), or(NA, NB)) :-
  normalise(A, NA),
  normalise(B, NB).

normalise(not(A), not(NA)) :- normalise(A, NA).

normalise(exactly_one_pos(L), and(Min1, not(Min2))) :-
  normalise(min_one_pos(L), Min1),
  normalise(min_two_pos(L),  Min2).

normalise(min_one_pos(L), R) :- list_to_or(L, R).

normalise(min_two_pos([A, B]), and(A, B)) :- !.
normalise(min_two_pos([H|T]), or(R, NT)) :-
  and_pairs(H, T, Pairs),
  list_to_or(Pairs, R),
  normalise(min_two_pos(T), NT).

%% helpers
list_to_or([H], H).
list_to_or([H|T], or(H, R)) :- list_to_or(T, R).

and_pairs(_, [], []).
and_pairs(A, [H|T], [and(A, H)|NT]) :- and_pairs(A, T, NT).



%% to_cnf(+Formula, -CNF).
to_cnf(F, LLCNF) :-
  normalise(F, NF),
  conjuctive_normalise(NF, CNF),
  split_conjunctions([CNF], [LCNF]),
  split_disjunctions(LCNF, LLCNF).

% Double Negation
conjuctive_normalise(not(not(A)), NA) :- !, conjuctive_normalise(A, NA).

% DeMorgan
conjuctive_normalise(not(and(A, B)), or(NA, NB)) :-
  conjuctive_normalise(not(A), NA),
  conjuctive_normalise(not(B), NB).
conjuctive_normalise(not(or(A, B)), and(NA, NB)) :-
  conjuctive_normalise(not(A), NA),
  conjuctive_normalise(not(B), NB).

% Distributive
conjuctive_normalise(or(A, and(B, C)), and(or(NA, NB), or(NA, NC))) :-
  conjuctive_normalise(A, NA), conjuctive_normalise(B, NB), conjuctive_normalise(C, NC), !.
conjuctive_normalise(or(and(B, C), A), and(or(NA, NB), or(NA, NC))) :-
  conjuctive_normalise(A, NA), conjuctive_normalise(B, NB), conjuctive_normalise(C, NC), !.

% Default
conjuctive_normalise(lit(A), A).
conjuctive_normalise(not(A), not(NA)) :- conjuctive_normalise(A, NA).
conjuctive_normalise(and(A, B), and(NA, NB)) :- conjuctive_normalise(A, NA), conjuctive_normalise(B, NB).
conjuctive_normalise(or(A, B), or(NA, NB)) :- conjuctive_normalise(A, NA), conjuctive_normalise(B, NB).



%% solve(+CNF).
solve(_CNF) :-
    true.
