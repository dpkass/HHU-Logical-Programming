:- use_module(library(plunit)).

%% mypermutation(+L, -P).

mypermutation([], []).
mypermutation([H|T], Perm) :-
    mypermutation(T, TPerm),
    insert_element(H, TPerm, Perm).

insert_element(H, T, [H|T]).
insert_element(H, [H2|T2], [H2|Temp]) :-
    insert_element(H, T2, Temp).


mypermutation_2([], []).
mypermutation_2([H|T], P) :-
    mypermutation_2(T, TP),
    insert_element_2([H|TP], P).

insert_element_2(L, P) :-
    L = [H|T],
    append(A,B,T),
    append(A,[H|B],P).


drop(L, N, R) :-
    drop(L, N, N, R).

drop([], _, _, []).
drop([_|T], 1, N, NL) :-
    drop(T, N, N, NL).

drop([H|T], C, N, [H|NL]) :-
    C > 1,
    NewC is C - 1,
    drop(T, NewC, N, NL).

:- begin_tests(permutations).

test(perm1) :-
    findall(P,mypermutation([1,2,3],P),Perms),
    !,
    memberchk([1,2,3],Perms),
    memberchk([3,1,2],Perms),
    memberchk([2,1,3],Perms).

test(perm2) :-
    findall(P,mypermutation([1,2],P),Perms),
    !,
    length(Perms,LP),
    LP == 2,
    memberchk([1,2],Perms),
    memberchk([2,1],Perms).

test(perm3) :-
    findall(P,mypermutation([1,2,3,4,5],P),Perms),
    !,
    memberchk([2,3,1,5,4],Perms),
    memberchk([2,3,1,4,5],Perms),
    memberchk([2,1,3,4,5],Perms),
    memberchk([1,3,2,5,4],Perms).

test(perm4, [all(L == [[]])]) :-
    mypermutation([], L).

test(noperm, [fail]) :-
    mypermutation([1,2,3],[2,1,1,3]).

test(noperm2, [fail]) :-
    mypermutation([1,2,3],[2,3]).

:- end_tests(permutations).

%% drop(+L, +N, -NL).

:- begin_tests(drop).

test(drop, [nondet,true(NL == [a,b,d,e,g,h,k])]) :- 
    drop([a,b,c,d,e,f,g,h,i,k], 3, NL).

test(drop2, [nondet,true(NL == [a,b])]) :- 
    drop([a,b,c], 3, NL).

test(drop3, [nondet,true(NL == [a])]) :- 
    drop([a], 2, NL).

test(drop4, [nondet,true(NL == [])]) :- 
    drop([], 2, NL).

test(drop_all, [nondet,all(NL == [[a,b,d,e,g,h,k]])]) :- 
    drop([a,b,c,d,e,f,g,h,i,k], 3, NL).

:- end_tests(drop).
