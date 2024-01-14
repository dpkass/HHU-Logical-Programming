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
to_cnf(_Formula, _CNF) :-
    true.

%% solve(+CNF).
solve(_CNF) :-
    true.
