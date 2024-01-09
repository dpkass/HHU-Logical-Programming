:- use_module(library(plunit)).


:- begin_tests(minimax).

test(minimax1, [true(BestTuple == (9, 0)),nondet]) :-
    minimax(max, 4, 0, [1, 2, 3, 5, 6, 7, 8, 9], BestTuple).

test(minimax2, [all(BestTuple == [(9, 0)])]) :-
    minimax(max, 6, 0, [1, 2, 3, 4, 5, 7, 8, 9], BestTuple).

test(minimax3, [all(BestTuple == [(6, 0)])]) :-
    minimax(max, 9, 0, [1, 2, 3, 4, 5, 6, 7, 8], BestTuple).

test(minimax4, [all(BestTuple == [(1, 0)])]) :-
    minimax(max, 14, 6, [1, 2, 3, 4, 7, 8], BestTuple).

test(minimax5, [all(BestTuple == [(8, 1)])]) :-
    minimax(max, 16, 7, [3, 4, 7, 8], BestTuple).

test(minimax6, [all(BestTuple == [(5, 0)])]) :-
    minimax(max, 10, 6, [2, 3, 4, 5, 7, 8], BestTuple).

test(minimax7, [all(BestTuple == [(4, 1)])]) :-
    minimax(max, 13, 11, [2, 4, 7, 8], BestTuple).

test(minimax8, [all(BestTuple == [(4, -1)])]) :-
    minimax(min, 11, 4, [4, 5, 6, 7, 8], BestTuple).

test(minimax9, [all(BestTuple == [(6, -1)])]) :-
    minimax(min, 9, 0, [1, 2, 3, 4, 5, 6, 7, 8], BestTuple).

test(minimax10, [all(BestTuple == [(4, -1)])]) :-
    minimax(min, 11, 4, [4, 5, 6, 7, 8], BestTuple).

:- end_tests(minimax).
