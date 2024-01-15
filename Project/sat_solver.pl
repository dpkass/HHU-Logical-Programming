:- module(sat_solver,[normalise/2, to_cnf/2, solve/1]).
:- use_module(library(lists)).

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

normalise(exactly_one_pos([Lit]), Lit).
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
  conjunctive_normalise(NF, CNF),
  split_conjunctions([CNF], [LCNF]),
  split_disjunctions(LCNF, LLCNF).

% Double Negation
conjunctive_normalise(not(not(A)), NA) :- !, conjunctive_normalise(A, NA).

% DeMorgan
conjunctive_normalise(not(and(A, B)), or(NA, NB)) :- !,
  conjunctive_normalise(not(A), NA),
  conjunctive_normalise(not(B), NB).
conjunctive_normalise(not(or(A, B)), and(NA, NB)) :- !,
  conjunctive_normalise(not(A), NA),
  conjunctive_normalise(not(B), NB).

% Distributive
conjunctive_normalise(or(A, and(B, C)), P) :-
  !, conjunctive_normalise(and(or(A, B), or(A, C)), P).
conjunctive_normalise(or(and(B, C), A), P) :-
  !, conjunctive_normalise(and(or(A, B), or(A, C)), P).

% Default
conjunctive_normalise(lit(A), A).
conjunctive_normalise(not(lit(A)), not(A)).
conjunctive_normalise(and(A, B), and(NA, NB)) :- conjunctive_normalise(A, NA), conjunctive_normalise(B, NB).
conjunctive_normalise(or(A, B), or(NA, NB)) :- conjunctive_normalise(A, NA), conjunctive_normalise(B, NB).

%% CNF to List Splitters
split_conjunctions(L, Res) :-
  maplist(split_conjunction, L, Res).

split_conjunction(P, [P]) :- var(P), !.
split_conjunction(and(A, B), LCNF) :-
  split_conjunction(A, LA),
  split_conjunction(B, LB),
  append(LA, LB, LCNF), !.
split_conjunction(P, [P]).

split_disjunctions(L, Res) :-
  maplist(split_disjunction, L, Res).

split_disjunction(P, [P]) :- var(P), !.
split_disjunction(or(A, B), LCNF) :-
  split_disjunction(A, LA),
  split_disjunction(B, LB),
  append(LA, LB, LCNF), !.
split_disjunction(P, [P]).



%% solve(+CNF).
solve(CNF) :-
  simplify(CNF, Simplified),
  dpll(Simplified).

dpll([]).
dpll(Clauses) :-
  select([Lit], Clauses, CLausesWOUnit), !,
  (var(Lit) -> Lit = true; Lit = not(false)),
  simplify(CLausesWOUnit, NewClauses),
  dpll(NewClauses).
dpll(Clauses) :-
  not(member([], Clauses)), % i didn't have nonmember pred
  select_and_set(Clauses),
  simplify(Clauses, NewClauses),
  dpll(NewClauses).

select_and_set([[true|_]|_]).
select_and_set([[false|_]|_]).
select_and_set([[Lit|_]|_]) :- nonvar(Lit), Lit = not(true).
select_and_set([[Lit|_]|_]) :- nonvar(Lit), Lit = not(false).

becomes_true(Clause) :- member_eq(true, Clause); member_eq(not(false), Clause).
remove_falsey(Clause, Clause2) :-
  delete_eq(Clause, false, Clause1),
  delete_eq(Clause1, not(true), Clause2).
simplify(Clauses, NewClauses) :-
  exclude(becomes_true, Clauses, NoTrueClauses),
  maplist(remove_falsey, NoTrueClauses, NewClauses).


%% helpers
delete_eq([], _, []) :- !.
delete_eq([H|T], E, R) :- H == E, delete_eq(T, E, R).
delete_eq([H|T], E, [H|R]) :- H \== E, delete_eq(T, E, R).

member_eq(_, []) :- fail.
member_eq(E, [H|_]) :- E == H, !.
member_eq(E, [_|T]) :- member_eq(E, T).