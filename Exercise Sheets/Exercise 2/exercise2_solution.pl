:- use_module(library(plunit)).

% task 1 - discussions

value(X,Interpretation,Value) :-
    atomic(X),
    member(X/Value,Interpretation).


value(and(A,B),I,Val) :-
    value(A,I,VA), value(B,I,VB),
    and(VA,VB,Val).


value(or(A,B),I,Val) :-
    value(A,I,VA), value(B,I,VB),
    or(VA,VB,Val).

value(not(A),I,Val) :-
    value(A,I,VA),
    not(VA,Val).
    
value(implies(A,B),I,Val) :- value(or(n(A),B),I,Val).


% task 2

person(siegfried).
person(krimhild).
person(gunther).
person(brunhild).
person(hagen).
person(alberich).


% 1. Siegfried loves Krimhild and likes Gunther.

loves(siegfried, krimhild).
likes(siegfried, gunther).

% 2. Krimhild loves Siegfried and hates Brunhild.
loves(krimhild, siegfried).
hates(krimhild, brunhild).

% 3. Gunther loves Brunhild and likes Krimhild and Hagen.
loves(gunther, brunhild).
likes(gunter, krimhild).
likes(gunter, hagen).

% 4. Brunhild hates Siegfried, Gunther and Krimhild.
hates(brunhild, siegfried).
hates(brunhild, gunther).
hates(brunhild, krimhild).

% 5. Hagen hates Siegfried and everyone who loves Siegfried.
hates(hagen, siegfried).
% hates(hagen, X) :- loves(X, siegfried), person(X).
hates(hagen, X) :- loves(X, siegfried).

% 6. Brunhild likes everyone who hates Siegfried.

% likes(brunhild, X) :- hates(X, siegfried), person(X).
likes(brunhild, X) :- hates(X, siegfried).

% 7. Alberich hates everyone but himself.
% hates(alberich, X) :- X \= alberich, person(X).
hates(alberich, X) :- X \= alberich.


% (1) likes(brunhild, X).
% (2) hates(X, siegfried).
% (3) loves(X,Y), loves(Y,X).

% task 3

can_talk(X) :-
    dog(X),
    live_in_pack(X).

dog(X) :- husky(X).
live_in_pack(X) :- husky(X).

husky(snowy).
poodle(snoopy).



% task 4

% last_but_one(+L, -E).

last_but_one([X,_], X).
last_but_one([_|T], E) :- last_but_one(T, E).

% my_infix(+I, +L).

my_infix(I, L) :-
    append(_, S, L),
    append(I, _, S),
    I \= [].

% Alternative:
% my_infix(I, L) :- my_prefix(I, L).
% my_infix(I, [_|L]) :- my_infix(I, L).

% my_suffix(+I, +L).

my_suffix(S, L) :-
    append(_, S, L),
    S \= [].

% my_prefix(+I, +L).

my_prefix(S, L) :-
    append(S, _, L),
    S \= [].

% del_element(+E, +L, -R).


%del_element(_, [], []).
%del_element(X, [H|T], L) :-
%    X \= H,
%    del_element(X, T, N),
%    append([H], N, L).

%del_element(X, [H|T], L) :-
%    X = H,
%    del_element(X, T, N),
%    append([], N, L).


% Simpler:

del_element(_, [], []).
del_element(X, [H|T], [H|N]) :-
    X \= H,
    del_element(X, T, N).


del_element(H, [H|T], L) :- del_element(H, T, L).

% task 5

% insert_at(+E, +L, +Index, -NL).

insert_at(E, [], Index, [E]) :- Index >= 1.
insert_at(E, L, 1, [E|L]).

insert_at(E, [P|L], Index, [P|NL]) :-
    Index > 1,
    NewIndex is Index - 1,
    insert_at(E, L, NewIndex, NL).




% Run all tests:
%   run_tests.
% Run all tests in a block:
%   run_tests(lists).
% Run a single test in a block:
%   run_tests(lists:is_a_list).
:- begin_tests(lists).

test(last_but_one, [nondet,true(L == c)]) :-
    last_but_one([a,b,c,d], L).

test(last_but_one_fail, [fail]) :-
    last_but_one([a], _).

test(last_but_one_fail2, [fail]) :-
    last_but_one([], _).

test(last_but_one_all, [all(L == [d])]) :-
    last_but_one([a,b,c,d,e], L).

test(infix_empty, [fail,nondet]) :-
    my_infix([], [a,b,c,d]).

test(infix1, [nondet]) :-
    my_infix([a,b,c,d], [a,b,c,d]).

test(infix2, [nondet]) :-
    my_infix([b,c], [a,b,c,d]).

test(infix3, [nondet]) :-
    my_infix([a,b,c], [a,b,c,d]).

test(infix4, [nondet]) :-
    my_infix([c,d], [a,b,c,d]).

test(infix_full, [nondet]) :-
    my_infix([a,b,c,d], [a,b,c,d]).

test(infix_fail, [fail,nondet]) :-
    my_infix([p,r,o,l,o,g], [a,b,c,d]).

test(suffix_empty, [fail,nondet]) :-
    my_suffix([], [a,b,c,d]).

test(suffix1, [nondet]) :-
    my_suffix([a,b,c,d], [a,b,c,d]).

test(suffix2, [nondet]) :-
    my_suffix([c,d], [a,b,c,d]).

test(suffix_fail, [fail,nondet]) :-
    my_suffix([a,b], [a,b,c,d]).

test(suffix_full, [nondet]) :-
    my_suffix([a,b,c,d], [a,b,c,d]).

test(prefix_empty, [fail,nondet]) :-
    my_prefix([], [a,b,c,d]).

test(prefix1, [nondet]) :-
    my_prefix([a,b,c,d], [a,b,c,d]).

test(prefix2, [nondet]) :-
    my_prefix([a,b], [a,b,c,d]).

test(prefix_fail1, [fail,nondet]) :-
    my_prefix([b,c], [a,b,c,d]).

test(prefix_full, [nondet]) :-
    my_prefix([a,b,c,d], [a,b,c,d]).

test(del_element, [nondet,true(R == [b,c,d])]) :-
    del_element(a,[a,b,c,d],R).

test(del_element_all, [all(R == [[b,c,d]])]) :-
    % evaluates all choicepoints
    del_element(a,[a,b,c,d],R).

test(del_element_twice, [nondet,true(R == [b,c,d])]) :-
    del_element(a, [a,b,c,d,a], R).

test(del_element_twice_failing, [fail]) :-
    del_element(a, [a,b,c,d,a], R), !, R == [b,c,d,a].

:- end_tests(lists).

:- begin_tests(insert_at).

test(insert_at,[nondet,true(R == [a,test,b,c])]) :-
    insert_at(test, [a,b,c], 2, R).

test(insert_at2,[nondet,true(R == [k,a,b])]) :-
    insert_at(k, [a,b], 1, R).

test(insert_at3,[nondet,true(R == [a,d])]) :-
    insert_at(d, [a], 3, R).

test(insert_at4,[nondet,true(R == [p])]) :-
    insert_at(p, [], 3, R).

test(insert_at5,[fail,nondet]) :-
    insert_at(a, [a,b,c], -3, _).

test(insert_at_all,[nondet,all(R == [[a,b,p,c,d,e]])]) :-
    % evaluates all choicepoints
    insert_at(p, [a,b,c,d,e], 3, R).

:- end_tests(insert_at).