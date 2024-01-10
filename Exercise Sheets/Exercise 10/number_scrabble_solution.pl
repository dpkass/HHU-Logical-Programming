number_scrabble :-
    format("A player has to reach a score of 15 to win the game.~n", []),
    number_scrabble(0, 0, [1,2,3,4,5,6,7,8,9]).

number_scrabble(15, _, _) :- !,
    format("You win!~n",[]).
number_scrabble(_, 15, _) :- !,
    format("Computer wins!~n",[]).
number_scrabble(PlayerScore, ComputerScore, _) :-
    PlayerScore > 15, ComputerScore > 15,
    format("Pat!~n",[]).
number_scrabble(PlayerScore, ComputerScore, Numbers) :-
    format("Current player score: ~w~nCurrent computer score: ~w~n", [PlayerScore, ComputerScore]),
    format("Choose a number from ~w~n", [Numbers]),
    read(Nr),
    select(Nr, Numbers, Rest),
    NewPlayerScore is PlayerScore + Nr,
    (   Rest == []
    ->  number_scrabble(NewPlayerScore, ComputerScore, [])
    ;   minimax(max, NewPlayerScore, ComputerScore, Rest, MaxTuple),
        MaxTuple = (ComputerNr,_),
        format("Compute chose number ~w~n", [ComputerNr]),
        NewComputerScore is ComputerScore + ComputerNr,
        select(ComputerNr, Rest, RestRest),
        number_scrabble(NewPlayerScore, NewComputerScore, RestRest)
    ).
number_scrabble(PlayerScore, ComputerScore, Numbers) :-
    number_scrabble(PlayerScore, ComputerScore, Numbers).

%%% Minimax implementation from here

%% minimax(+CurrentPlayer, +PlayerScore, +ComputerScore, +Numbers, -BestTuple).
% -BestTuple is a tuple containing the best next move (number to choose) as well as its score.
minimax(max, _, 15, _, Out) :-
    !,
    Out = (_,1).
minimax(min, 15, _, _, Out) :-
    !,
    Out = (_,-1).
minimax(_, _, _, [], Out) :-
    !,
    Out = (_,0).
minimax(CurrentPlayer, PlayerScore, ComputerScore, Numbers, BestTuple) :-
    findall((Score,Number),
            (select(Number, Numbers, RestNrs),
             minimax_rec(CurrentPlayer, PlayerScore, ComputerScore, RestNrs, Number, (_, Score))),
            ListOfTuples),
    (   CurrentPlayer == max
    ->  max_member(InversedBestTuple, ListOfTuples)
    ;   min_member(InversedBestTuple, ListOfTuples)
    ),
    InversedBestTuple = (BestScore, BestNumber),
    BestTuple = (BestNumber, BestScore).

minimax_rec(max, PlayerScore, ComputerScore, RestNrs, Number, BestTuple) :-
    NewComputerScore is ComputerScore + Number,
    minimax(min, PlayerScore, NewComputerScore, RestNrs, BestTuple).
minimax_rec(min, PlayerScore, ComputerScore, RestNrs, Number, BestTuple) :-
    NewPlayerScore is PlayerScore + Number,
    minimax(max, NewPlayerScore, ComputerScore, RestNrs, BestTuple).

:- use_module(library(plunit)).


:- begin_tests(minimax).

test(minimax1, [true(BestTuple == (9, 0)),nondet]) :-
    minimax(max, 4, 0, [1, 2, 3, 5, 6, 7, 8, 9], BestTuple).

test(minimax2, [all(BestTuple == [(9, 0)])]) :-
    minimax(max, 6, 0, [1, 2, 3, 4, 5, 7, 8, 9], BestTuple).

test(minimax3, [all(BestTuple == [(6, 0)])]) :-
    minimax(max, 9, 0, [1, 2, 3, 4, 5, 6, 7, 8], BestTuple).

test(minimax4, [all(BestTuple == [(1, 0)])]) :-
    minimax(max, 14, 6, [1, 2, 3, 4, 7, 8], BestTuple).

test(minimax5, [all(BestTuple == [(8, 1)])]) :-
    minimax(max, 16, 7, [3, 4, 7, 8], BestTuple).

test(minimax6, [all(BestTuple == [(5, 0)])]) :-
    minimax(max, 10, 6, [2, 3, 4, 5, 7, 8], BestTuple).

test(minimax7, [all(BestTuple == [(4, 1)])]) :-
    minimax(max, 13, 11, [2, 4, 7, 8], BestTuple).

test(minimax8, [all(BestTuple == [(4, -1)])]) :-
    minimax(min, 11, 4, [4, 5, 6, 7, 8], BestTuple).

test(minimax9, [all(BestTuple == [(6, -1)])]) :-
    minimax(min, 9, 0, [1, 2, 3, 4, 5, 6, 7, 8], BestTuple).

test(minimax10, [all(BestTuple == [(4, -1)])]) :-
    minimax(min, 11, 4, [4, 5, 6, 7, 8], BestTuple).

:- end_tests(minimax).
