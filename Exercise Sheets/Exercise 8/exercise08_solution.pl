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

dfs(_, Puzzle, [Puzzle]) :- solved(Puzzle).

dfs(History, Puzzle, [Puzzle|Path]) :-
    s(Puzzle, NewPuzzle),
    \+ member(NewPuzzle, History),
    dfs([NewPuzzle|History], NewPuzzle, Path).


%% s(+Puzzle, -NewPuzzle).

s(Puzzle, NewPuzzle) :-
    get_coord(x, Puzzle, Coords),
    get_next_coords(Coords, NextCoords),
    move(Puzzle, Coords, NextCoords, NewPuzzle).


get_coord(Symbol, c(R1, _, _, _), (PosX, 0)) :- nth0(PosX, R1, Symbol).
get_coord(Symbol, c(_, R2, _, _), (PosX, 1)) :- nth0(PosX, R2, Symbol).
get_coord(Symbol, c(_, _, R3, _), (PosX, 2)) :- nth0(PosX, R3, Symbol).
get_coord(Symbol, c(_, _, _, R4), (PosX, 3)) :- nth0(PosX, R4, Symbol).


get_next_coords((CurX, CurY), (NewCurX, CurY)) :-
    CurX > 0,
    NewCurX is CurX - 1.

get_next_coords((CurX, CurY), (NewCurX, CurY)) :-
    CurX < 3,
    NewCurX is CurX + 1.

get_next_coords((CurX, CurY), (CurX, NewCurY)) :-
    CurY > 0,
    NewCurY is CurY - 1.

get_next_coords((CurX, CurY), (CurX, NewCurY)) :-
    CurY < 3,
    NewCurY is CurY + 1.


move(Puzzle, Coords, NextCoords, NewPuzzle) :-
    get_coord(TempSymbol, Puzzle, NextCoords),
    set_field(Puzzle, Coords, TempSymbol, TempPuzzle),
    set_field(TempPuzzle, NextCoords, x, NewPuzzle).

set_field(c(R1, R2, R3, R4), (X, 0), Symbol, c(NewR1, R2, R3, R4)) :-
    replace_at(R1, Symbol, X, NewR1).

set_field(c(R1, R2, R3, R4), (X, 1), Symbol, c(R1, NewR2, R3, R4)) :-
    replace_at(R2, Symbol, X, NewR2).

set_field(c(R1, R2, R3, R4), (X, 2), Symbol, c(R1, R2, NewR3, R4)) :-
    replace_at(R3, Symbol, X, NewR3).

set_field(c(R1, R2, R3, R4), (X, 3), Symbol, c(R1, R2, R3, NewR4)) :-
    replace_at(R4, Symbol, X, NewR4).

% Alternative for line 62 - 72 with =.. operator
%set_field(Puzzle, (X, Y), Symbol, NewPuzzle) :-
%    Puzzle =.. PuzzleAsList,
%    NewY is Y + 1,
%    nth0(NewY, PuzzleAsList, R),
%    replace_at(R, Symbol, X, NewR),
%    replace_at(PuzzleAsList, NewR, NewY, NewPuzzleAsList),
%    NewPuzzle =.. NewPuzzleAsList.


replace_at([], Symbol, _, [Symbol]).
replace_at([_|T], Symbol, 0, [Symbol|T]).
replace_at([H|T], Symbol, Pos, [H|NewT]) :-
    NewPos is Pos - 1,
    replace_at(T, Symbol, NewPos, NewT).


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
