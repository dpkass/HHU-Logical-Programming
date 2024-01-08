:- use_module(library(plunit)).

% ------------------ Task 3 ------------------
% Implement a Prolog predicate solve_idfs(+Puzzle) which solves the 15 puzzle2 using iterative
% deepening.
% The data structure representing a puzzle is a term c/4 with four lists. Each list contains a number
% between 1 and 15 or the atom x which represents the empty field in the puzzle. For instance:
% c([x, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]).
% Start by implementing a predicate s(+Puzzle, -NextPuzzle) which computes a possible next state
% NextPuzzle for a given puzzle. It should be possible to enumerate all subsequent puzzle states
% by backtracking.

solved(c([x, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15])).

% The following code is just an example on how to start. You can also implement it differently if you want to.
% lst/1 has been explained in the lecture: generates lists with one more variable element when backtracking; starts with the empty list
lst([]).
lst([_|T]) :-
    lst(T).

%% solve_idfs(+Puzzle).
solve_idfs(Puzzle) :-
    lst(Path),
    dfs([], Puzzle, Path).

%% dfs(+History, +Puzzle, -PathToSolution).
dfs(_, P, [P]) :-
  solved(P).
dfs(H, P, [P|Path]) :-
  \+ solved(P),
  s(P, NP),
  \+ member(NP, H),
  dfs([P|H], NP, Path).

%% s(+Puzzle, -NewPuzzle).
s(P, NP) :- vertical(P, NP).
s(c(R, R2, R3, R4), c(NR, R2, R3, R4)) :- succ(R, NR).
s(c(R1, R, R3, R4), c(R1, NR, R3, R4)) :- succ(R, NR).
s(c(R1, R2, R, R4), c(R1, R2, NR, R4)) :- succ(R, NR).
s(c(R1, R2, R3, R), c(R1, R2, R3, NR)) :- succ(R, NR).

vertical(c([H1|R1],[H2|R2],[H3|R3],[H4|R4]), c([NH1|R1],[NH2|R2],[NH3|R3],[NH4|R4])) :-
  succ([H1, H2, H3, H4], [NH1, NH2, NH3, NH4]).
vertical(c([H1|R1],[H2|R2],[H3|R3],[H4|R4]), c([H1|NR1],[H2|NR2],[H3|NR3],[H4|NR4])) :-
  vertical(c(R1, R2, R3, R4), c(NR1, NR2, NR3, NR4)).

succ([x, A, B, C], [A, x, B, C]).
succ([A, x, B, C], [x, A, B, C]).
succ([A, x, B, C], [A, B, x, C]).
succ([A, B, x, C], [A, x, B, C]).
succ([A, B, x, C], [A, B, C, x]).
succ([A, B, C, x], [A, B, x, C]).


:- begin_tests(successor_states).

test(successor1, [nondet]) :-
    s(c([x, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]), NewState),
    NewState = c([1, x, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]).

test(successor2, [nondet]) :-
    s(c([x, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]), NewState),
    NewState = c([4, 1, 2, 3], [x, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]).

test(successor3, [nondet]) :-
    s(c([1, 2, x, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]), NewState),
    NewState = c([1, x, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]).

test(successor4, [nondet]) :-
    s(c([1, 2, x, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]), NewState),
    NewState = c([1, 2, 3, x], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]).

test(successor5, [nondet]) :-
    s(c([1, 2, x, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]), NewState),
    NewState = c([1, 2, 6, 3], [4, 5, x, 7], [8, 9, 10, 11], [12, 13, 14, 15]).

:- end_tests(successor_states).

:- begin_tests(solve_idfs).

test(simple) :-
    time(solve_idfs(c([4, 1, 2, 3], [8, 5, 6, 7], [x, 9, 10, 11], [12, 13, 14, 15]))) , !.

test(simple2,[nondet]) :-
    time(solve_idfs(c([4, 1, 2, 3], [x, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]))).

test(simple3,[nondet]) :-
    time(solve_idfs(c([4, 1, 2, 3], [8, 5, 6, 7], [9, x, 10, 11], [12, 13, 14, 15]))).

test(simple4,[nondet]) :-
    time(solve_idfs(c([4, 1, 2, 3], [8, 5, 6, 7], [9, 10, 14, 11], [12, 13, x, 15]))).

test(simple5,[nondet]) :-
    time(solve_idfs(c([4, 1, 2, 3], [8, 5, 6, 7], [x, 10, 14, 11], [9, 12, 13, 15]))).

test(simple6,[nondet]) :-
    time(solve_idfs(c([4, 1, 2, 3], [8, 5, 6, 7], [10, 14, 13, 11], [9, x, 12, 15]))).


% These tests will probably be slow using idfs but they should terminate.
% The A* algorithm is able to solve the puzzle faster (will be discussed next week).

test(complex1,[nondet]) :-
    time(solve_idfs(c([4, 1, 2, 3], [8, 5, 6, 7], [14, x, 13, 11], [10, 9, 12, 15]))).

test(complex2,[nondet]) :-
    time(solve_idfs(c([4, 1, 2, 3], [8, x, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15]))).

test(complex3,[nondet]) :-
    time(solve_idfs(c([4, 2, x, 3], [8, 1, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15]))).

:- end_tests(solve_idfs).
