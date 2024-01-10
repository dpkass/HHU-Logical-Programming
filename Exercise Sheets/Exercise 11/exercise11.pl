:- use_module(library(plunit)).

% ------------------ Task 3 ------------------
% Predicates can be dynamically added to the knowledge base using asserta/1 (insert top) or
% assertz/1 (insert bottom). Dynamic predicates can be removed using retract/1 or retractall/1.
% • Check the behavior of asserta/1 and retract/1 when backtracking.
% • Implement the predicates btasserta/1 and btretract/1 which behave like asserta/1 and
%   retract/1 but revert their effect when backtracking.

btasserta(A) :- asserta(A).
btasserta(A) :- retract(A), fail. % Force backtracking

btretract(A) :- retract(A).
btretract(A) :- asserta(A), fail.

:- begin_tests(mysum).

test(sum1, [true(S == 0)]) :-
    mysum([], S).

test(sum2, [true(S == 6)]) :-
    mysum([1,2,3], S).

test(sum3, [true(S == 15)]) :-
    mysum([1,2,3,4,5], S).

test(sum_all1, [all(S == [15])]) :-
    mysum([1,2,3,4,5], S).

test(sum_all2, [all(S == [0])]) :-
    mysum([], S).

:- end_tests(mysum).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% used for the "higher_order" tests
add(Cst, Val, Sum) :- Sum is Val + Cst.

:- begin_tests(higher_order).

test(reduce1, [fail]) :-
    reduce(add, [], _).

test(reduce2, [true(R == 6)]) :-
    reduce(add, [1,2,3], R).

test(reduce3, [true(R == 1)]) :-
    reduce(add, [1], R).

:- end_tests(higher_order).

:- begin_tests(interleave).

test(interleave1, [true(L == [a, 1, b, 2])]) :-
    interleave([[a,b],[1,2]], L).

test(interleave2, [true(L == [a, 1, x, b, 2, y, c, 3, z])]) :-
    interleave([[a,b,c],[1,2,3],[x,y,z]], L).

test(interleave3, [true(L == [])]) :-
    interleave([], L).

test(interleave4, [true(L == [1,3])]) :-
    interleave([[1,2],[3]], L).

test(interleave_all1, [all(L == [[a, 1, b, 2]])]) :-
    interleave([[a,b],[1,2]], L).

test(interleave_all2, [all(L == [[1,3]])]) :-
    interleave([[1,2],[3]], L).

:- end_tests(interleave).