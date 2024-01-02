:- use_module(library(plunit)).

% ------------------ Task 2 ------------------
% Implgunther, brunhildng facts iKrimhild

% 1. Siegfried loves Krimhild and likes Gunther.
loves(siegfried, krimhild).
likes(siegfried, gunther).

% 2. Krimhild loves Siegfried and hates Brunhild.
loves(krimhild, siegfried).
hates(krimhild, brunhild).

% 3. Gunther loves Brunhild and likes Krimhild and Hagen.
loves(gunther, brunhild).
likes(gunther, krimhild).
likes(gunther, hagen).

% 4. Brunhild hates Siegfried, Gunther and Krimhild.
hates(brunhild, siegfried).
hates(brunhild, gunther).
hates(brunhild, krimhild).

% 5. Hagen hates Siegfried and everyone who loves Siegfried.
hates(hagen, siegfried).
hates(hagen, P) :- loves(P, siegfried).

% 6. Brunhild likes everyone who hates Siegfried.
likes(brunhild, P) :- hates(P, siegfried).

% 7. Alberich hates everyone but himself.
hates(alberich, P) :- P \= alberich.


% Answer the following questions with Prolog queries

% 1. Who does Brunhild like?
likes(brunhild, X).

% 2. Who hates Siegfried?
hates(X, siegfried).

% 3. Which couples could be formed? (persons who love each other)
loves(A, B), loves(B, A), B \= A.



% ------------------ Task 3 ------------------
% Translate the following statements to First-Order Logic

% 1. The following applies to all dogs: If they live in packs, then they can talk.
% ∀x: dog(x) ∧ in_pack(x) --> talk(x)

% 2. All huskies are dogs and live in packs.
% ∀x: husky(x) --> dog(x) ∧ in_pack(x)

% 3. Snowy is a husky.
% husky(snowy)



% ------------------ Task 4 ------------------
% For tasks b) to d), infix, suffix and prefix should be non-empty. Use append/3.

% a)
% last_but_one(+L, -E).
% Return the last but one element of the list L in E.
last_but_one([X, _], X).
last_but_one([_|X], E) :- last_but_one(X, E).

% b)
% my_infix(+I, +L).
% Test whether I is an infix of the list L.
my_infix(I, L) :- my_prefix(I, L).
my_infix(I, [_|L]) :- my_infix(I, L).
% meaning prefix or prefix of suffix

% c)
% my_suffix(+I, +L).
% Test whether S is a suffix of the list L.
my_suffix(I, L) :- append(_, I, L), I \= [].

% d)
% my_prefix(+I, +L).
% Test whether P is a prefix of the list L.
my_prefix(I, L) :- append(I, _, L), I \= [].

% e)
% del_element(+E, +L, -R).
% Delete all occurences of E in the list L and return the resulting list in R.
del_element(_, [], []).
del_element(E, [E|T], R) :- del_element(E, T, R).
del_element(E, [H|T], [H|R]) :- H \= E, del_element(E, T, R).


% ------------------ Task 5 ------------------

% insert_at(+E, +L, +Index, -NL).
% Implement a predicate insert_at/4 which inserts an element to a list at a given index. If the
% index is larger than the size of the list, the element should be added to the end of the list.
insert_at(E, L, 1, [E|L]).
insert_at(E, [], Index, [E]) :- Index > 1.
insert_at(E, [H|T], Index, [H|TNL]) :- Index > 1, NewIndex is Index - 1, insert_at(E, T, NewIndex,
TNL).



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