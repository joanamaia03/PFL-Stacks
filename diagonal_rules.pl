:- use_module(library(lists)).

% assigns the different possible diagonal directions to each set of initial and final coordinate pairs
define_diagonal_direction([InitialI, InitialJ], [FinalI, FinalJ], Direction):-
    AuxI is FinalI - InitialI,
    AuxJ is FinalJ - InitialJ,
    (AuxI =:= AuxJ, AuxI > 0 -> Direction = 'diagonal_right_up';
    AuxI =:= - AuxJ, AuxI < 0 -> Direction = 'diagonal_right_down';
    AuxI =:= - AuxJ, AuxI > 0 -> Direction = 'diagonal_left_up';
    AuxI =:= AuxJ, AuxI < 0 -> Direction = 'diagonal_left_down';
    Direction = 'not_diagonal').

% checks if the initial piece is between two oponent pieces that are on that diagonal
diagonal_rule_right_up(Initial,'diagonal_right_up', Board, Player):-
    nth1(1, Initial, I),
    nth1(2, Initial, J),
    NewI is I-1,
    occupied_by_opponent(Board, NewI, J, Player),
    NewJ is J+1,
    occupied_by_opponent(Board, I, NewJ, Player).

% checks if the initial piece is between two oponent pieces that are on that diagonal
diagonal_rule_right_down(Initial,'diagonal_right_down', Board, Player):-
    nth1(1, Initial, I),
    nth1(2, Initial, J),
    NewJ is J+1,
    occupied_by_opponent(Board, I, NewJ, Player),
    NewI is I+1,
    occupied_by_opponent(Board, NewI, J, Player).

% checks if the initial piece is between two oponent pieces that are on that diagonal
diagonal_rule_left_up(Initial,'diagonal_left_up', Board, Player):-
    nth1(1, Initial, I),
    nth1(2, Initial, J),
    NewI is I-1,
    occupied_by_opponent(Board, NewI, J, Player),
    NewJ is J-1,
    occupied_by_opponent(Board, I, NewJ, Player).

% checks if the initial piece is between two oponent pieces that are on that diagonal
diagonal_rule_left_down(Initial,'diagonal_left_down', Board, Player):-
    nth1(1, Initial, I),
    nth1(2, Initial, J),
    NewJ is J-1,
    occupied_by_opponent(Board, I, NewJ, Player),
    NewI is I+1,
    occupied_by_opponent(Board, NewI, J, Player).

% checks if the piece/stack can move diagonally, making sure that it is not between two of the opponents pieces that are on that diagonal
diagonal_rules(Initial, Final, Board, Player):-
    define_diagonal_direction(Initial, Final, Direction),
    \+ diagonal_rules_directions(Initial, Direction, Board, Player).

diagonal_rules_directions(Initial, Direction, Board, Player):-
    diagonal_rule_right_up(Initial,'diagonal_right_up', Board, Player);
    diagonal_rule_right_down(Initial,'diagonal_right_down', Board, Player);
    diagonal_rule_left_up(Initial,'diagonal_left_up', Board, Player);
    diagonal_rule_left_down(Initial,'diagonal_left_down', Board, Player).

occupied_by_opponent(Board, I, J, Player) :-
    (Player = 1,
    matrix(Board, I, J, Value),
    Value < 0);
    (Player = 2,
    matrix(Board, I, J, Value),
    Value > 0).

% Accessing a value in the board.
% I is the index for the rows and J for the columns.
matrix(Board, I, J, Value):-
    nth1(I, Board, Row),
    nth1(J, Row, Value).    