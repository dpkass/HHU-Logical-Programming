:- use_module(library(plunit)).

% task 3

compress([], []).
compress([A], [A]).

compress([A,B|T], L) :-
    A == B, % no unification, see compress_var
    compress([A|T], L).
compress([A,B|T], [A|L]) :-
    A \== B,
    compress([B|T], L).

% task 4

inorder(nil, []).
% inorder(node(V, nil, nil), [V]).
inorder(node(V, Left, Right), L) :-
    inorder(Left, L1),
    inorder(Right, L2),
    append(L1, [V|L2], L).

preorder(nil, []).
preorder(node(V, Left, Right), L) :-
    preorder(Left, L1),
    preorder(Right, L2),
    append([V|L1], L2, L).

postorder(nil, []).
postorder(node(V, Left, Right), L) :-
    postorder(Left, L1),
    postorder(Right, L2),
    append(L2, [V], L3),
    append(L1, L3, L).

% task 5

t(L, NL) :-
    t(L, [], NL). % start with [] as accumulator

t([], L, L). % base case: accumulator (already reversed list) is returned as result
t([H|T], A, NL) :- % add element at the beginning of A
    t(T, [H|A], NL). % apply to tail recursively, provide reversed list with A as accumulator


% accumulator is the result that is already computed

% t([1,2,3,4], L). => L = [4,3,2,1]
% t implements reverse


my_reverse([], []).
my_reverse([H|T], R) :-
    my_reverse(T, TRes),
    append(TRes, [H], R).

sum([], 0).
sum([H|T], R) :-
    sum(T, TmpR),
    R is H + TmpR.

sum_acc(L, Sum) :-
    sum_acc(L, 0, Sum).
    
sum_acc([], Sum, Sum).
sum_acc([H|T], Acc, Sum) :-
    TmpAcc is Acc + H,
    sum_acc(T, TmpAcc, Sum).



% task 6

%euclid_recursive(x,y):
%    if y = 0:
%        return x
%    if x = 0:
%        return y
%    if x > y:
%        return euclid_recursive(x - y, y)
%    return euclid_recursive(x, y - x)

gcd(X, 0, X).
gcd(0, Y, Y).

gcd(X, Y, GCD) :-
    X > 0,
    Y > 0,
    X > Y,
    NewX is X-Y,
    gcd(NewX, Y, GCD).
    
gcd(X, Y, GCD) :-
    X > 0,
    Y > 0,
    X =< Y,
    NewY is Y-X,
    gcd(X, NewY, GCD).

coprime(X,Y) :-
    gcd(X,Y,GCD),
    GCD == 1.


% Additional

% element_at(1, [3,4,2,1], R) => R=3.

element_at(1, [H|_], H).
element_at(N, [_|T], R) :-
    N > 1,
    NextN is N - 1,
    element_at(NextN, T, R).


% remove_at(1, [1,2,3,4], R) => R = [2,3,4].

remove_at(1, [_|T], T).
remove_at(Index, [H|T], [H|R]) :-
    Index > 0,
    NewIndex is Index - 1,
    remove_at(NewIndex, T, R).




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

:- end_tests(gcd).


:- begin_tests(element_at).


test(element_at_base, [nondet,all(R == [12])]) :-
    element_at(1, [12,3,4,5,6], R).

test(element_at_base2, [nondet,all(R == [4])]) :-
    element_at(1, [4], R).

test(element_at1, [nondet,all(R == [4])]) :-
    element_at(4, [1,2,3,4], R).

test(element_at2, [nondet,all(R == [2])]) :-
    element_at(6, [5,7,83,2,1,2,3,4], R).


:- end_tests(element_at).


:- begin_tests(remove_at).


test(remove_at_base, [nondet,all(R == [[3,4,5,6]])]) :-
    remove_at(1, [12,3,4,5,6], R).

test(remove_at_base2, [nondet,all(R == [[]])]) :-
    remove_at(1, [4], R).

test(remove_at1, [nondet,all(R == [[1,2,3]])]) :-
    remove_at(4, [1,2,3,4], R).

test(remove_at2, [nondet,all(R == [[5,7,83,2,1,3,4]])]) :-
    remove_at(6, [5,7,83,2,1,2,3,4], R).


:- end_tests(remove_at).