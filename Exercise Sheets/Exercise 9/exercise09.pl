:- use_module(library(plunit)).

% ------------------ Task 2 ------------------

% a) Implement a predicate mymaplist/3 which behaves like the corresponding implementation in
% SWI-Prolog (see maplist/3).

% mymaplist/3 receives a predicate in the first argument, a list in the second argument, and
% returns a new list in the third argument which contains all elements after applying the
% predicate to the list’s elements.

mymaplist(_, [], []).
mymaplist(F, [H|R], [HM|RM]) :-
  call(F, H, HM),
  mymaplist(F, R, RM).


% b) Implement a predicate myinclude/3 which receives a predicate in the first argument and a
% list in the second argument. In the third argument, the predicate should return a new list of all
% elements of the list provided in the second argument for which the predicate is true. The order
% of elements should be kept.

myinclude(_, [], []).
myinclude(F, [H|R], [H|RF]) :-
  call(F, H),
  myinclude(F, R, RF).
myinclude(F, [H|R], RF) :-
  \+ call(F, H),
  myinclude(F, R, RF).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% used for the "higher_order" tests
even(X) :- X mod 2 =:= 0.
add(Cst, Val, Sum) :- Sum is Val + Cst.

:- begin_tests(higher_order).

test(maplist1, true(Res == [3,5,7])) :-
    mymaplist(add(1), [2,4,6], Res).

test(maplist2, true(Res == [4,6,8])) :-
    mymaplist(add(2), [2,4,6], Res).

test(maplist3, true(Res == [za,zb,zc])) :-
    mymaplist(atom_concat(z), [a,b,c], Res).

test(maplist4, true(Res == [[z,a],[z,b],[z,c]])) :-
    mymaplist(append([z]), [[a],[b],[c]], Res).

test(include1, true(Res == [a,b,c])) :-
    myinclude(ground, [_,a,_,_,_,b,c], Res).

test(include2, true(Res == [a,d,b,c])) :-
    myinclude(ground, [_,a,_,_,_,d,b,c], Res).

test(include3, true(Res == [2,4,6,8,10])) :-
    myinclude(even, [1,2,3,4,5,6,7,8,9,10,11], Res).

test(include4, all(Res == [[]])) :-
    myinclude(even, [1,3,5,7,9], Res).

test(include5, all(Res == [[]])) :-
    myinclude(var, [], Res).

test(include6, true(Res == [A,B,C,D])) :-
    myinclude(var, [A,a,b,c,B,C,D], Res).

test(include_fail, [fail]) :-
    myinclude(even, [1,2,3,4], []).

:- end_tests(higher_order).

:- begin_tests(greater_nrs_only).

test(greater_nrs_only1, [all(X == [[1,1,5,4]])]) :-
    greater_nrs_only(0, [1,1,5,-3,-6,-7,4], X).

test(greater_nrs_only2, [all(X == [[5,4]])]) :-
    greater_nrs_only(1, [1,a,1,f(-5),5,-3,4], X).

test(greater_nrs_only3, [all(X == [[]])]) :-
    greater_nrs_only(6, [3,1,k,4], X).

test(greater_nrs_only4, [all(X == [[]])]) :-
    greater_nrs_only(0, [a,b,c,f(a),e,f], X).

test(greater_nrs_only5, [all(X == [[-9,-8,-7]])]) :-
    greater_nrs_only(-10, [-11,-10,-9,-8,-7], X).

test(greater_nrs_only6, [all(X == [[]])]) :-
    greater_nrs_only(0, [0], X).

test(greater_nrs_only_fail1, [fail]) :-
    greater_nrs_only(6, [3,1,k,4], [3,1,4]).

test(greater_nrs_only_fail2, [fail]) :-
    greater_nrs_only(1, [3,1,3,4,a,b,1,f(b),4], []).

test(greater_nrs_only_fail3, [fail]) :-
    greater_nrs_only(1, [3,1,3,4,a,b,1,f(b),4], [3,3,4]).

test(greater_nrs_only_fail4, [fail]) :-
    greater_nrs_only(0, [0], X),
    X = [0].

:- end_tests(greater_nrs_only).

