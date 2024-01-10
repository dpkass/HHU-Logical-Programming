:- use_module(library(plunit)).



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