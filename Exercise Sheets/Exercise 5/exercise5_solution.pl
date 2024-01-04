:- use_module(library(plunit)).

% task 2

system_heats :-
    cover_closed,
    power_on.

power_on :-
    lamp_shines.

lamp_shines.
cover_closed.


% task 3

is_atomic_expr(Term) :- atom(Term).

is_literal(A) :- is_atomic_expr(A).
is_literal(not(A)) :- is_atomic_expr(A).

simplify_expr(A, A) :- is_literal(A).
simplify_expr(or(A,B), or(SimplifiedA,SimplifiedB)) :-
    simplify_expr(A, SimplifiedA),
    simplify_expr(B, SimplifiedB).
simplify_expr(and(A,B), and(SimplifiedA,SimplifiedB)) :-
    simplify_expr(A, SimplifiedA),
    simplify_expr(B, SimplifiedB).

simplify_expr(not(not(A)), Simplified) :- simplify_expr(A, Simplified).
simplify_expr(not(and(A,B)), or(SimplifiedA, SimplifiedB)) :-
    simplify_expr(not(A), SimplifiedA),
    simplify_expr(not(B), SimplifiedB).
simplify_expr(not(or(A,B)), and(SimplifiedA, SimplifiedB)) :-
    simplify_expr(not(A), SimplifiedA),
    simplify_expr(not(B), SimplifiedB).


is_clause(Term) :-
    simplify_expr(Term, Simplified),
    is_disjunction(Simplified).

is_disjunction(A) :- is_literal(A).
is_disjunction(or(A,B)) :-
    is_disjunction(A),
    is_disjunction(B).

is_horn_clause(Term) :-
    is_clause(Term),
    simplify_expr(Term, Simplified),
    max_one_positive(Simplified).

max_one_positive(Term) :- is_literal(Term).
max_one_positive(or(A,B)) :-
    no_positive(A),
    max_one_positive(B).
max_one_positive(or(A,B)) :-
    max_one_positive(A),
    no_positive(B).

no_positive(not(A)) :-
    is_atomic_expr(A).

no_positive(or(A,B)) :-
    no_positive(A),
    no_positive(B).

is_denial(Term) :-
    simplify_expr(Term, Simplified),
    no_positive(Simplified).


% task 5

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


range_1(N, []) :- N =< 1.
range_1(N, L) :-
    N > 1,
    NewN is N - 1,
    range_1(NewN, NL),
    append(NL, [NewN], L).

phi(M, N) :-
    range_1(M, Interval),
    phi_count(Interval, M, N).

phi_count([], _, 0).
phi_count([H|T], M, Count) :-
    coprime(H,M),
    phi_count(T, M, PreviousCount),
    Count is PreviousCount + 1.

phi_count([H|T], M, Count) :-
    \+ coprime(H,M),
    phi_count(T, M, Count).


:- begin_tests(prop_logic).

test(is_atomic_expr,[nondet]) :-
    is_atomic_expr(a),
    is_atomic_expr(b),
    is_atomic_expr(c),
    \+ is_atomic_expr(f(a)),
    \+ is_atomic_expr(_X),
    \+ is_atomic_expr(not(a)).

test(is_literal,[nondet]) :-
    is_literal(a),
    is_literal(b),
    \+ is_literal(_X),
    \+ is_literal(f(a)),
    is_literal(not(a)),
    \+ is_literal(not(not(a))).

test(simplify_expr_basic_atomic, [nondet,true(S == a)]) :-
    % print('Test: '),
    % print(simplify_expr_basic_atomic), nl,
    simplify_expr(a, S).

test(simplify_expr_basic_not, [nondet,true(S == not(a))]) :-
    simplify_expr(not(a), S).

test(simplify_expr_basic_or, [nondet,true(S == or(a, b))]) :-
    simplify_expr(or(a, b), S).

test(simplify_expr_basic_and, [nondet,true(S == and(a, b))]) :-
    simplify_expr(and(a, b), S).

test(simplify_expr_basic_and_2, [nondet,true(S == and(a, b))]) :-
    simplify_expr(and(not(not(a)), not(not(b))), S).

test(simplify_expr_negation1, [nondet,true(R == a)]) :-
    simplify_expr(not(not(a)), R).

test(simplify_expr_negation2, [nondet,true(R == not(a))]) :-
    simplify_expr(not(not(not(a))), R).

test(simplify_expr_negation3, [nondet,true(R == a)]) :-
    simplify_expr(not(not(not(not(a)))), R).

test(simplify_expr_de_morgan1, [nondet,true(R == or(not(a), not(b)))]) :-
    simplify_expr(not(and(a, b)), R).

test(simplify_expr_de_morgan1, [nondet,true(R == and(not(a), not(b)))]) :-
    simplify_expr(not(or(a, b)), R).

test(simplify_expr_de_morgan_extended, [nondet,true(R == or(a, b))]) :-
    simplify_expr(not(and(not(a), not(b))), R).

test(is_clause, [nondet]) :-
    is_clause(a),
    is_clause(not(a)),
    is_clause(or(a, b)),
    is_clause(or(a, or(b, c))),
    is_clause(or(not(a), or(b, not(c)))),
    \+ is_clause(or(a, and(b, c))),
    \+ is_clause(and(a, b)),
    is_clause(or(or(a, b), c)).

test(is_clause_simplify, [nondet]) :-
    is_clause(not(and(a, b))).

test(is_horn_clause_simple, [nondet]) :-
    is_horn_clause(a),
    is_horn_clause(b).

test(is_horn_clause, [nondet]) :-
    is_horn_clause(not(a)),
    is_horn_clause(or(a, not(b))),
    is_horn_clause(or(not(a), b)),
    is_horn_clause(or(not(a), not(b))),
    \+ is_horn_clause(or(a, b)),
    \+ is_horn_clause(and(a, not(b))),

    is_horn_clause(or(a, or(not(b), not(c)))),
    \+ is_horn_clause(or(a, or(not(b), c))),

    is_horn_clause(or(or(not(a), b), not(c))).

test(is_horn_clause_simplify, [nondet]) :-
    is_horn_clause(not(and(a, b))).

test(is_denial, [nondet]) :-
    is_denial(or(not(a),not(b))).

test(is_denial_fail, [fail]) :-
    is_denial(or(a,not(b))).

:- end_tests(prop_logic).

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
