:- use_module(library(plunit)).


% Task 2

%a

%mymaplist(_, [], R) :- !, R = [].
mymaplist(_, [], []).
mymaplist(Pred, [H|T], [HR|TR]) :-
    call(Pred, H, HR),
    mymaplist(Pred, T, TR).

% additional

%max(X,Y,Y) :- X =< Y, !.
%max(X,Y,X).

% fixed
max(X,Y,R) :- X =< Y, !, R=Y.
max(X,Y,X).


% b

myinclude(_, [], R) :- !, R = [].
myinclude(Pred, [H|T], R) :-
    myinclude(Pred, T, TR),
    (call(Pred, H) -> R = [H|TR]; R = TR).


myinclude_2(_, [], R) :- !, R = [].
myinclude_2(Pred, [H|T], R) :-
    myinclude_2(Pred, T, TR),
    \+ call(Pred, H),
    R = TR.

myinclude_2(Pred, [H|T], R) :-
    myinclude_2(Pred, T, TR),
    call(Pred, H),
    R = [H|TR].



% Task 4
% a

%greater_nrs_only(_, [], R) :- !, R = [].
%greater_nrs_only(N, [H|T], R) :-
%    number(H),
%    H > N, !,
%    R = [H|TR],
%    greater_nrs_only(N, T, TR).

%greater_nrs_only(N, [_|T], R) :-
%    greater_nrs_only(N, T, R).


% with if-then-else
%greater_nrs_only(N, [H|T], R) :-
%    (number(H), H > N ->
%    (R = [H|TR],
%    greater_nrs_only(N, T, TR));
%    greater_nrs_only(N, T, R)).


% b

greater(N,I) :-
    number(I),
    N < I.

greater_nrs_only(N,L,R) :-
    include(greater(N), L, R).


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

