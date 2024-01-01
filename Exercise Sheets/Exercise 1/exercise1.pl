:- use_module(library(plunit)).

% Load Prolog file from terminal: swipl exercise1.pl

female(dolly).
female(haba).
female(doerte).
female(pauline).

male(friedrich).
male(gunter).
male(peter).

parent(haba, dolly).
parent(haba, gunter).
parent(friedrich, dolly).
parent(friedrich, gunter).
parent(gunter, peter).
parent(gunter, pauline).
parent(doerte, peter).
parent(doerte, pauline).

% TODO

% Start Testcases: run_tests(sheep).

:- begin_tests(sheep,[]).

test(sheep,[nondet]) :-
    sheep(friedrich) ,
    sheep(doerte).

test(sheep_failing,[fail]) :-
    sheep(sebastian).

test(father,[nondet]) :-
    father(friedrich,dolly).

test(father_2,[nondet]) :-
    father(gunter,peter).

test(father_failing,[fail]) :-
    father(doerte,pauline).

test(father_failing_2,[fail]) :-
    father(friedrich,peter).

test(mother,[nondet]) :-
    mother(haba,dolly).

test(mother_failing,[fail]) :-
    mother(gunter,pauline).

test(mother_failing_2,[fail]) :-
    mother(friedrich,peter).

test(ancestor,[nondet]) :-
    ancestor(friedrich,peter).

test(ancestor_2,[nondet]) :-
    ancestor(friedrich,pauline).

test(ancestor_failing,[fail]) :-
    ancestor(dolly,peter).

test(ancestor_failing_2,[fail]) :-
    ancestor(peter,peter).

test(first_mother, [nondet, true(C = haba)]) :-
    mother(C, dolly).

test(one_mother, [all(C = [haba])]) :-
    mother(C, dolly).

test(male_sheeps, [all(S = [friedrich,gunter,peter])]) :-
    male(S).

:- end_tests(sheep).
