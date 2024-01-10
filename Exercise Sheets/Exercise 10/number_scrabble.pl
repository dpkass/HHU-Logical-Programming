:- use_module(library(plunit)).
:- use_module(library(lists)).

% game
start(pos(max, 0, 0, [1,2,3,4,5,6,7,8,9])).

end(Pos) :- eval(Pos, X), X \= 0.
end(pos(_, A, B, Options)) :- Options = [].

eval(pos(_, _, 15, _), 1).
eval(pos(_, 15, _, _), -1).
eval(pos(_, A, B, _), 0) :- A \= 15, B \= 15.

move(pos(max, Min, Max, Options), Action, pos(min, Min, NMax, NOptions)) :-
  take_one(Options, NOptions, Action),
  NMax is Max + Action.
move(pos(min, Min, Max, Options), Action, pos(max, NMin, Max, NOptions)) :-
  take_one(Options, NOptions, Action),
  NMin is Min + Action.


% minimax

minimax(Pos, (end, Eval)) :-
  end(Pos), eval(Pos, Eval).

minimax(Pos, (Action, Eval)) :-
  not(end(Pos)),
  findall(
      (ChildEval, ChildAction),
      (move(Pos, ChildAction, NPos),
      minimax(NPos, (_, ChildEval))),
      ActionEvals),
  best_action(Pos, ActionEvals, Action, Eval).

best_action(pos(Player, _, _, _), ActionEvals, Action, Eval) :-
  Player = max ->
  max_member((Eval, Action), ActionEvals);
  min_member((Eval, Action), ActionEvals).

% helper
take_one(L, NL, Val) :-
  append(L2, [Val|L3], L),
  append(L2, L3, NL).

:- begin_tests(minimax).

test(minimax1, [true(BestTuple == (9, 0)),nondet]) :-
    minimax(pos(max, 4, 0, [1, 2, 3, 5, 6, 7, 8, 9]), BestTuple).

test(minimax2, [all(BestTuple == [(9, 0)])]) :-
    minimax(pos(max, 6, 0, [1, 2, 3, 4, 5, 7, 8, 9]), BestTuple).

test(minimax3, [all(BestTuple == [(6, 0)])]) :-
    minimax(pos(max, 9, 0, [1, 2, 3, 4, 5, 6, 7, 8]), BestTuple).

test(minimax4, [all(BestTuple == [(1, 0)])]) :-
    minimax(pos(max, 14, 6, [1, 2, 3, 4, 7, 8]), BestTuple).

test(minimax5, [all(BestTuple == [(8, 1)])]) :-
    minimax(pos(max, 16, 7, [3, 4, 7, 8]), BestTuple).

test(minimax6, [all(BestTuple == [(5, 0)])]) :-
    minimax(pos(max, 10, 6, [2, 3, 4, 5, 7, 8]), BestTuple).

test(minimax7, [all(BestTuple == [(4, 1)])]) :-
    minimax(pos(max, 13, 11, [2, 4, 7, 8]), BestTuple).

test(minimax8, [all(BestTuple == [(4, -1)])]) :-
    minimax(pos(min, 11, 4, [4, 5, 6, 7, 8]), BestTuple).

test(minimax9, [all(BestTuple == [(6, -1)])]) :-
    minimax(pos(min, 9, 0, [1, 2, 3, 4, 5, 6, 7, 8]), BestTuple).

test(minimax10, [all(BestTuple == [(4, -1)])]) :-
    minimax(pos(min, 11, 4, [4, 5, 6, 7, 8]), BestTuple).

:- end_tests(minimax).
