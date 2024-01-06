:- use_module(library(plunit)).

% ------------------ Task 2 ------------------
% Define the following statements in propositional logic:

% 1. The system heats if the cover is closed and the power is on.
% cover ∧ power --> system_heats

% 2. If the lamp shines, the power is on.
% lamp --> power

% 3. The lamp shines.
% lamp

% 4. The cover is closed.
% cover

% cover ∧ power --> system_heats
% => ¬(cover ∧ power) ∨ system_heats
% => ¬cover ∨ ¬power ∨ system_heats
% lamp --> power
% => ¬lamp ∨ power

% KNF: {{¬cover, ¬power, system_heats}, {¬lamp, power}, {lamp}, {cover}}
% abbreviate and enumerate:
% 1. {¬c, ¬p, s}
% 2. {¬l, p}
% 3. {l}
% 4. {c}
% 5. {¬s} (denied query)

% 6 := 1 + 5 = {¬c, ¬p, s} + {¬s} ⊨ {¬c, ¬p}
% 7 := 4 + 6 ⊨ {¬p}
% 8 := 2 + 3 ⊨ {p}
% 9 := 7 + 8 ⊨ {}



% ------------------ Task 3 ------------------
% In this exercise we want to analyse and manipulate propositional formulae. We represent
% propositional formulae as terms in Prolog while propositional statements are represented as
% Prolog atoms. For instance, the propositional formula a ∧ b ∨ c is represented as the term
% or(and(a, b), c) in Prolog. Implement the following predicates:

% a) is_atomic_expr(+Term) is true iff Term is a propositional statement.
is_atomic_expr(T) :- atom(T).

% b) is_literal(+Term) is true iff Term is a literal (positive or negative propositional statement).
is_literal(T) :- is_atomic_expr(T).
is_literal(not(T)) :- is_atomic_expr(T).

% c) simplify_expr(+Term, -Simplified) simplifies a given propositional formula by applying the
% following rules:
%   • Remove double negations: ¬¬A ≡ A
%   • DeMorgan’s Laws
%     (a) ¬(A ∧ B) ≡ ¬A ∨ ¬B
%     (b) ¬(A ∨ B) ≡ ¬A ∧ ¬B

simplify_expr(T, T) :- is_literal(T).

simplify_expr(and(A, B), and(SA, SB)) :-
  simplify_expr(A, SA),
  simplify_expr(B, SB).
simplify_expr(or(A, B), or(SA, SB)) :-
  simplify_expr(A, SA),
  simplify_expr(B, SB).

simplify_expr(not(not(T)), S) :- simplify_expr(T, S).
simplify_expr(not(and(A, B)), or(SA, SB)) :-
  simplify_expr(not(A), SA),
  simplify_expr(not(B), SB).
simplify_expr(not(or(A, B)), and(SA, SB)) :-
  simplify_expr(not(A), SA),
  simplify_expr(not(B), SB).



% d) is_clause(+Term) is true iff Term is a clause after simplification.
is_clause(T) :-
  simplify_expr(T, S),
  is_disjunction(S).

is_disjunction(T) :- is_literal(T).
is_disjunction(or(A, B)) :-
  is_disjunction(A),
  is_disjunction(B).


% e) is_horn_clause(+Term) is true iff Term is a horn-clause after simplification.
is_horn_clause(T) :-
  simplify_expr(T, S),
  max_one_pos(S).

max_one_pos(T) :- is_literal(T).
max_one_pos(or(A, B)) :-
  max_one_pos(A),
  no_pos(B).
max_one_pos(or(A, B)) :-
  no_pos(A),
  max_one_pos(B).

no_pos(not(T)) :- is_atomic_expr(T).
no_pos(or(A, B)) :-
  no_pos(A),
  no_pos(B).


% f) is_denial(Term) is true iff Term is a denial after simplification.
is_denial(T) :-
  simplify_expr(T, S),
  no_pos(T).



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
    simplify_expr(a, S).

test(simplify_expr_basic_not, [nondet,true(S == not(a))]) :-
    simplify_expr(not(a), S).

test(simplify_expr_basic_or, [nondet,true(S == or(a, b))]) :-
    simplify_expr(or(a, b), S).

test(simplify_expr_basic_and, [nondet,true(S == and(a, b))]) :-
    simplify_expr(and(a, b), S).

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
