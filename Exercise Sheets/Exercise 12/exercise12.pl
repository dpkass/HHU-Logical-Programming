:- use_module(library(plunit)).

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