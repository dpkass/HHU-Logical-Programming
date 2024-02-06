:- use_module(library(plunit)).

% Exercise 2 (Difference Lists)
% As you have seen in the lecture, difference lists can improve performance of list operations.
% In particular, we are able to add an element to the end of a difference list in constant time
% while usual Prolog lists show linear time complexity for this operation. In general, you can
% choose an arbitrary structure to store a difference list. A common representation uses the
% functor -/2 such as [a,b|R]-R.

% a) Implement a predicate toDL(+List, ?DiffList, ?Diff) which transforms a Prolog list into a
% difference list. The first argument is the Prolog list and the second and third arguments
% represent the difference list.

toDL(L, DL, D) :- append(L, D, DL).

% b) Implement a predicate dlconcat(+DL1, +DL2, -DLOut) which concatenates two difference lists
% represented as X-Y.

dlconcat(DL1-A, A-B, DL1-B).

% c) Implement a predicate dlmember/2 which behaves as lists:member/2 but for difference lists.

dlmember(_, L-R) :- L == R, !, false.
dlmember(V, [H|T]-R) :- H = V; dlmember(V, T-R).



% Exercise 3 (Definite Clause Grammars (DCGs))
% Implement a (dcg-)parser which is able to detect if a term is correctly bracketed. If this is
% the case, the amount of bracket pairs should be counted.

dynamic(parse_pars).

parse_pars(S, N) :- string_codes(S, T), parse_pars(N, T, []).

parse_pars(0) --> "".
parse_pars(N) --> "(", parse_pars(NN), ")", {N is NN + 1}.
parse_pars(N) --> "{", parse_pars(NN), "}", {N is NN + 1}.
parse_pars(N) --> "[", parse_pars(NN), "]", {N is NN + 1}.
parse_pars(N) --> "<", parse_pars(NN), ">", {N is NN + 1}.

:- begin_tests(to_dl).

test(to_dl1, [nondet,true(X = [a,b,c|R])]) :-
    toDL([a,b,c], X, R).

test(to_dl2, [nondet]) :-
    toDL([a,b], [a,b,c],[c]).

test(to_dl3, [true(X = [a,b,c,d])]) :-
    toDL([a,b], X, [c,d]).

:- end_tests(to_dl).

:- begin_tests(dlconcat).

test(dlconcat1, [nondet,true([List,A] = [[a,b,c,d,e,f|B]-B,[d,e,f|B]])]) :-
    dlconcat([a,b,c|A]-A, [d,e,f|B]-B, List).

test(dlconcat2, [nondet,true([List,A] = [[a,b,c|B]-B,B])]) :-
    dlconcat([a,b,c|A]-A, B-B, List).

test(dlconcat3, [nondet,true([List,A] = [B-B,B])]) :-
    dlconcat(A-A, B-B, List).

test(dlconcat4, [nondet,true([List,A] = [[a,b,c,f(a),"1","2","3"|B]-B,["1","2","3"|B]])]) :-
    dlconcat([a,b,c,f(a)|A]-A, ["1","2","3"|B]-B, List).

:- end_tests(dlconcat).

:- begin_tests(dlmember).

test(dlmember1, [all(A == [a,b,c])]) :-
    dlmember(A, [a,b,c|R]-R).

test(dlmember2, [all(A == [[a,b,c],1,3])]) :-
    dlmember(A, [[a,b,c],1,3|R]-R).

test(dlmember_fail1, [fail]) :-
    dlmember(1, [a,b,c|R]-R).

test(dlmember_fail2, [fail]) :-
    dlmember(a, R-R).

:- end_tests(dlmember).


:- begin_tests(parser).

test(parse1, [all(N == [1])]) :-
    parse_pars(`()`, N).

test(parse2, [all(N == [4])]) :-
    parse_pars(`{([<>])}`, N).

test(parse3, [nondet,all(N == [1])]) :-
    parse_pars("()", N).

test(parse4, [nondet,all(N == [4])]) :-
    parse_pars("{([<>])}", N).

test(parse5, [nondet,all(N == [0])]) :-
    parse_pars(``, N).

test(parse6, [nondet,all(N == [23])]) :-
    parse_pars(`([{((((((((<[{[<{([{<<<>>>}])}>]}]>))))))))}])`, N).

test(parse7, [nondet,all(N == [3])]) :-
    parse_pars("((()))", N).

test(parse_fail1, [fail]) :-
    parse_pars(`{([<>)}`,_) , !.

test(parse_fail2, [fail]) :-
    parse_pars(`([{<>}<]>)`,_) , !.

test(parse_fail3, [fail]) :-
    parse_pars(`([{<([{]})>}])`,_) , !.

:- end_tests(parser).


:- begin_tests(sudoku_parser).

test(parse_sudoku1, [all(Sudoku = [[[_,_,_,_],[_,_,_,_],[_,_,_,_],[_,_,_,_]]])]) :-
    parse_sudoku('./sudoku_puzzles/sud4x4.txt', Sudoku).

test(parse_sudoku2, [all(Sudoku = [[[_,3,_,_,_,_,_,_,_],[_,_,_,1,9,5,_,_,_],[_,_,8,_,_,_,_,6,_],[8,_,_,_,6,_,_,_,_],[4,_,_,8,_,_,_,_,1],[_,_,_,_,2,_,_,_,_],[_,6,_,_,_,_,2,8,_],[_,_,_,4,1,9,_,_,5],[_,_,_,_,_,_,_,7,_]]])]) :-
    parse_sudoku('./sudoku_puzzles/sud9x9.txt', Sudoku).

:- end_tests(sudoku_parser).

print_sudoku(Sudoku) :-
    nl,
    print_sudoku_rows(Sudoku).

print_sudoku_rows([]) :- nl.
print_sudoku_rows([Row|T]) :-
    print_sudoku_row(Row),
    print_sudoku_rows(T).

print_sudoku_row([]) :- nl.
print_sudoku_row([Nr|T]) :-
    format("~w ", [Nr]),
    print_sudoku_row(T).