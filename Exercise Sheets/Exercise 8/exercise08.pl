:- use_module(library(plunit)).

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

%% s(+Puzzle, -NewPuzzle).




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

/*
% These tests will probably be slow using idfs but they should terminate.
% The A* algorithm is able to solve the puzzle faster (will be discussed next week).

test(complex1,[nondet]) :-
    time(solve_idfs(c([4, 1, 2, 3], [8, 5, 6, 7], [14, x, 13, 11], [10, 9, 12, 15]))).

test(complex2,[nondet]) :-
    time(solve_idfs(c([4, 1, 2, 3], [8, x, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15]))).

test(complex3,[nondet]) :-
    time(solve_idfs(c([4, 2, x, 3], [8, 1, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15]))).*/

:- end_tests(solve_idfs).
