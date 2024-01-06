:- use_module(library(plunit)).

% ------------------ Task 2 ------------------
% Find the most general unificator (mgu) σ for each of the following pairs of terms if they can
% be unified. Do not use a Prolog interpreter to solve the tasks. It is sufficient to state the
% results for σ.

% a) [] and []
% σ = {}

% b) [H|T] and [1,2,[3]]
% σ = {H/1, T/[2,[3]]

% c) [X,Y] and [c|[[a,b]]]
% σ = {X/c, Y/[a,b]}

% d) 2 and 1+1
% not unifiable: different functions

% e) r(a,X) and r(Y,r(a,b))
% σ = {Y/a, X/r(a,b)}

% f) f(X,Y) and f(Y,f(X))
% not unifiable with occurs, else: σ = {X/f(X), Y/f(X)}

% g) n(a,b) and f(X,Y)
% not unifiable: different functions

% h) f(a,b) and f(X,X)
% not unifiable: constant overlap

% i) [1,2|E]-E and [X,Y,F|G]-[a,b,c]
% σ = {X/1, Y/2, E/[a,b,c], F/a, G/[b,c]}



% ------------------ Task 3 ------------------
% Decide for each pair of substitutions (unificators) which substitution is more general.

% Use the following definition: The substitution Θ is more general than Φ if there exists σ such
% that Φ = Θσ.

% a) {X/a} and {X/a,Y/a}
% Left

% b) {X/Y} and {X/a,Y/a}
% Left

% c) {X/Y,Z/a} and {X/a,Y/a,Z/a}
% Left

% d) {X/1,Y/Z} and {X/1,Y/2,Z/3}
% Not equal

% e) {X/a} and {X/b}
% Not equal



% ------------------ Task 4 ------------------
% Implement a predicate mypermutation(+L, -P) which calculates all permutations of a list L. It
% should be possible to enumerate all permutations using backtracking.

mypermutation([], []).
mypermutation([H|L], P) :-
  mypermutation(L, P2),
  insert(P2, H, P).


insert(L, E, P) :-
  append(A, B, L),
  append(A, [E|B], P).



% ------------------ Task 5 ------------------
% Implement a predicate drop(+L, +N, -NL) which drops every N-th element from L and returns the
% resulting list in NL.

drop(L, N, NL) :- drop(L, 1, N, NL).

drop([], _, _, []).
drop([_|L], P, N, NL) :- % why did drop([_|L], N, N, NL) not work?
  P = N,
  drop(L, 1, N, NL).
drop([H|L], P, N, [H|NL]) :-
  P < N,
  NewP is P + 1,
  drop(L, NewP, N, NL).



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
