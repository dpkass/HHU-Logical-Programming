:- use_module(library(plunit)).

% ------------------ Task 2 ------------------
% Let K := {{A,C},{¬C,B},{¬B,¬C,A}} be a set of clauses. Prove by contradiction and resolution
% that the statement A holds.

% {{A,C},{¬C,B},{¬B,¬C,A},{¬A}}
% {A,C} ∧ {¬A} --> {C}
% {{A,C},{¬C,B},{¬B,¬C,A},{¬A},{C}}
% {¬B,¬C,A} ∧ {¬A} --> {¬B,¬C}
% {{A,C},{¬C,B},{¬B,¬C,A},{¬A},{C},{¬B,¬C}}
% {¬B,¬C} ∧ {C} --> {¬B}
% {{A,C},{¬C,B},{¬B,¬C,A},{¬A},{C},{¬B,¬C},{¬B}}
% {¬C,B} ∧ {¬B} --> {¬C}
% {{A,C},{¬C,B},{¬B,¬C,A},{¬A},{C},{¬B,¬C},{¬B},{¬C}}
% {C} ∧ {¬C} --> 0

% task 3

% task 4

% task 6

:- begin_tests(compress).

test(compress1,[all(X == [[]])]) :-
    compress([], X).

test(compress2, [nondet,true(X == [a,b,c,a,d,e])]) :-
    compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e], X).

test(compress3, [nondet,true(X == [a,b,c,a,d,e])]) :-
    compress([a,a,a,a,b,c,c,a,d,e], X).

test(compress4, [fail]) :-
    compress([a,a,a,a,b,c,c,a,d,e], X),
    X = [a,b,c,d,e].

test(compress5, [fail]) :-
    compress([a,a,a,a,b,c,c,a,d,e], X),
    X = [a,a,a,a,b,c,c,a,d,e].

test(compress6, [fail]) :-
    compress([a,a,a,a,b,c,c], [a,b,c,c]).

test(compress7, [fail]) :-
    compress([a,a,a,a,b,c,c], [a,a,a,a,b,c,c]).

test(compress8, [all(X == [[a,b,c,a,d,e]])]) :-
    compress([a,a,a,a,b,c,c,a,d,e], X).

test(compress9, [all(X == [[a]])]) :-
    compress([a,a,a,a], X).

test(compress10, [all(X == [[1,2,1,2,3,1,2,3,4]])]) :-
    compress([1,1,1,1,2,2,1,1,2,2,3,3,1,1,2,2,3,3,4,4], X).

test(compress_var, [true(L == [a,Var,b,c,d])]) :-
    compress([a,Var,b,c,d], L).

:- end_tests(compress).

:- begin_tests(binary_trees).

test(inorder_empty, [nondet,true(L == [])]) :-
    inorder(nil, L).

test(inorder1, [nondet,true(L == [3,2,1,5,4])]) :-
    inorder(node(1,node(2,node(3,nil,nil),nil),node(4,node(5,nil,nil),nil)), L).

test(inorder2, [nondet,true(L == [1,4,3,2,5])]) :-
    inorder(node(2,node(3,node(4,node(1,nil,nil),nil),nil),node(5,nil,nil)), L).

test(inorder3, [nondet,true(L == [2])]) :-
    inorder(node(2,nil,nil), L).

test(inorder4, [nondet,all(L == [[1,4,3,2,6,7,8]])]) :-
    inorder(node(2,node(3,node(4,node(1,nil,nil),nil),nil),node(8,node(7,node(6,nil,nil),nil),nil)), L).

test(preorder_empty, [nondet,true(L == [])]) :-
    preorder(nil, L).

test(preorder1, [nondet,true(L == [1,2,3,4,5])]) :-
    preorder(node(1,node(2,node(3,nil,nil),nil),node(4,node(5,nil,nil),nil)), L).

test(preorder2, [nondet,true(L == [2,3,4,1,5])]) :-
    preorder(node(2,node(3,node(4,node(1,nil,nil),nil),nil),node(5,nil,nil)), L).

test(preorder3, [nondet,true(L == [2])]) :-
    preorder(node(2,nil,nil), L).

test(preorder4, [nondet,all(L == [[2,3,4,1,8,7,6]])]) :-
    preorder(node(2,node(3,node(4,node(1,nil,nil),nil),nil),node(8,node(7,node(6,nil,nil),nil),nil)), L).

test(postorder_empty, [nondet,true(L == [])]) :-
    postorder(nil, L).

test(postorder1, [nondet,true(L == [3,2,5,4,1])]) :-
    postorder(node(1,node(2,node(3,nil,nil),nil),node(4,node(5,nil,nil),nil)), L).

test(postorder2, [nondet,true(L == [1,4,3,5,2])]) :-
    postorder(node(2,node(3,node(4,node(1,nil,nil),nil),nil),node(5,nil,nil)), L).

test(postorder3, [nondet,true(L == [2])]) :-
    postorder(node(2,nil,nil), L).

test(postorder4, [nondet,all(L == [[1,4,3,6,7,8,2]])]) :-
    postorder(node(2,node(3,node(4,node(1,nil,nil),nil),nil),node(8,node(7,node(6,nil,nil),nil),nil)), L).

:- end_tests(binary_trees).

:- begin_tests(gcd).

test(gcd, [nondet,true(GCD == 9)]) :-
    gcd(36, 63, GCD).

test(gcd2, [nondet,true(GCD == 1)]) :-
    gcd(1253, 23, GCD).

test(gcd3, [nondet,true(GCD == 12)]) :-
    gcd(324, 12, GCD).

test(gcd4, [nondet,true(GCD == 2)]) :-
    gcd(368, 34, GCD).

test(gcd_function, [nondet,true(GCD == 9)]) :-
    GCD is gcd(36, 63).

test(gcd_function2, [nondet],true(GCD == 1)) :-
    GCD is gcd(1253, 23).

test(gcd_all, [all(GCD == [9])]) :-
    gcd(36, 63, GCD).

test(gcd_all2, [all(GCD == [12])]) :-
    gcd(324, 12, GCD).

test(coprime, [nondet]) :-
    coprime(1253, 23).

test(coprime2, [nondet]) :-
    coprime(1234423, 123).

test(coprime3, [fail]) :-
    coprime(324, 12).

test(coprime4, [fail]) :-
    coprime(36, 63).

test(range, [nondet,true(L == [1,2,3,4,5,6,7,8,9])]) :-
    range_1(10, L).

test(range2, [nondet,true(L == [1,2,3,4])]) :-
    range_1(5, L).

test(range3, [nondet,true(L == [])]) :-
    range_1(1, L).

test(range4, [nondet,true(L == [])]) :-
    range_1(0, L).

test(range5, [nondet,true(L == [])]) :-
    range_1(-1, L).

test(range_all, [nondet,all(L == [[]])]) :-
    range_1(-1, L).

test(range_all2, [nondet,all(L == [[1,2,3,4,5,6,7,8,9]])]) :-
    range_1(10, L).

test(phi, [nondet,true(L == 4)]) :-
    phi(10, L).

test(phi2, [nondet,true(L == 8)]) :-
    phi(15, L).

test(phi3, [nondet,true(L == 288)]) :-
    phi(323, L).

test(phi_all, [nondet,all(L == [288])]) :-
    phi(323, L).

test(phi_all2, [nondet,all(L == [8])]) :-
    phi(15, L).

:- end_tests(gcd).

