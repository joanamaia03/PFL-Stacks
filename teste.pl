:- use_module(library(lists)).

% base case where line equals 1.
% replace element on line 1 and a specific column.
% replace_matrix(NewValue, 1, Column, Board, Result).
replace_matrix(New, 1, X,[R|Rows], [Result|Rows]):-
    replace_element(New, X, R, Result).

% replace element on a specific line and column. 
% replace_matrix(NewValue, Line, Column, Board, Result).
replace_matrix(New, Y, X, [R|Rows], [R|Result]):-
    Y>1,
    Y1 is Y-1,
    replace_matrix(New, Y1, X, Rows, Result).

% base case where column equals 1.
% replace the first element on column 1.
% we ignore the head of the board and substitute by the new value.
replace_element(New, 1, [_|Tail], [New|Tail]).

% the board is traversed until we find the column we want.
replace_element(New, X, [H|Tail],[H|Result]):-
    X>1,
    X1 is X-1,
    replace_element(New,X1,Tail,Result).     
% ---------------------------------------------------

% checks if two coordenates (I1, I2) and (J1, J2) are adjacent.
check_if_adjacent(I1,J1,I2,J2):-
    (I1=:=I2, abs(J1-J2)=:=1);
    (J1 =:= J2, abs(I1 - I2) =:= 1);
    (abs(I1-I2)=:=1, abs(J1-J2)=:=1).

% checks if two coordenates (I1, I2) and (J1, J2) are different from eachother.
check_if_different(I1,J1,I2,J2):-
    \+ [I1,J1] = [I2,J2].      

% --------------------------------------------------------

in_bounds(I, J) :-
    I < 8,
    J < 6,
    I > 0,
    J > 0.

% Moves up a stack.
move_up(InitialI, InitialJ, FinalI, FinalJ):-
    FinalI is InitialI - 1,
    FinalJ is InitialJ,
    in_bounds(FinalI, FinalJ).

% Moves down a stack.
move_down(InitialI, InitialJ, FinalI, FinalJ):-
    FinalI is InitialI + 1,
    FinalJ is InitialJ,
    in_bounds(FinalI, FinalJ).

% Moves left a stack.
move_left(InitialI, InitialJ, FinalI, FinalJ):-
    FinalI is InitialI,
    FinalJ is InitialJ - 1,
    in_bounds(FinalI, FinalJ).

% Moves right a stack.
move_right(InitialI, InitialJ, FinalI, FinalJ):-
    FinalI is InitialI,
    FinalJ is InitialJ + 1,
    in_bounds(FinalI, FinalJ).

% Moves diagonally to the left and upwards.
move_diagonal_left_up(InitialI, InitialJ, FinalI, FinalJ):-
    FinalI is InitialI - 1,
    FinalJ is InitialJ - 1,
    in_bounds(FinalI, FinalJ).

% Moves diagonally to the left and downwards.
move_diagonal_left_down(InitialI, InitialJ, FinalI, FinalJ):-
    FinalI is InitialI + 1,
    FinalJ is InitialJ + 1,
    in_bounds(FinalI, FinalJ).   

% Moves diagonally to the right and upwards.
move_diagonal_right_up(InitialI, InitialJ, FinalI, FinalJ):-
    FinalI is InitialI - 1,
    FinalJ is InitialJ + 1,
    in_bounds(FinalI, FinalJ).

% Moves diagonally to the right and downwards.
move_diagonal_right_down(InitialI, InitialJ, FinalI, FinalJ):-
    FinalI is InitialI + 1,
    FinalJ is InitialJ - 1,
    in_bounds(FinalI, FinalJ).        

% -----------------------------------------------------------------------------------

% Accessing a value in the board.
% I is the index for the rows and J for the columns.
matrix(Board, I, J, Value):-
    nth1(I, Board, Row),
    nth1(J, Row, Value).

% Gives a list of all the pieces from a player that are on the board.
find_players_pieces(Board, Player, Pieces):-
    findall([I, J], is_players_piece(Board, I, J, Player), Pieces).

% To confirm if the piece that is going to move correspond to a piece from the player that is trying to move it.
is_players_piece(Board, I, J, Player):-
    (Player = 1,
    matrix(Board, I, J, Value),
    Value > 0);
    (Player is 2,
    matrix(Board, I, J, Value),
    Value < 0). 

% -------------------------------------------------------------------------------    

% valid_moves(+GameState, +Player, -ListOfMoves). 
% GameState = [Board, PiecesIG1, PiecesRM1, PiecesIG2, PiecesRM2, Turn, CountNeg, CountPos].
% Turn can be 1 or 2, depending on what player should play.
valid_moves(GameState, ListOfMoves):-
    nth1(6, GameState, Player),
    nth1(1, GameState, Board),
    find_players_pieces(Board,Player,Pieces),
    get_list_of_moves(Board, Player, Pieces, ListOfMoves).

% ---------------------------------------------------------------------------------------------------

% gets a list with all the possible moves from a player
get_list_of_moves(Board, Player, Pieces, ListOfMoves):-
    findall([Initial, Final], (
        nth1(_, Pieces, Initial), % The coordenates of the existent pieces of the player are saved in the initial, it iterates over the pieces.
        nth1(FinalI, Board, I_list),
        nth1(FinalJ, I_list, _Space),
        in_bounds(FinalI, FinalJ),
        Final = [FinalI, FinalJ],
        Initial = [InitialI, InitialJ],
        check_if_adjacent(InitialI, InitialJ, FinalI, FinalJ),
        check_if_different(InitialI, InitialJ, FinalI, FinalJ),
        diagonal_rules(Initial, Final, Board, Player),
        \+ occupied_by_opponent(Board, FinalI, FinalJ, Player),
        matrix(Board, InitialI, InitialJ, Value1),
        matrix(Board, FinalI, FinalJ, Value2),
        can_friendly(Value1, Value2),
        % gets the value from the initial cell and checks the allowance.
        matrix(Board, InitialI, InitialJ, Value),
        absolute_value(Value, NewValue),
        Allowance is 4 - NewValue,
        distance(InitialI, InitialJ, FinalI, FinalJ, Distance),
        Allowance =:= Distance
    ), ListOfMoves).

% -------------------------------------------------------------------------------------------------------------------------------------------------------

occupied_by_opponent(Board, I, J, Player) :-
    (Player = 1,
    matrix(Board, I, J, Value),
    Value < 0);
    (Player = 2,
    matrix(Board, I, J, Value),
    Value > 0). 

% -----------------------------------------------------------------------

% Checks if it can go to a friendly space (may be occupied by a space that is not occupied or that is occupied by a friendly space).
% As a stack cannot be bigger than 4, checks if the sum is less than 5.
% Both numbers must be in its absolute value.
can_friendly(ValueMover, ValueThere) :-
    (ValueMover + ValueThere) < 5.

% ---------------------------------------------------------------------------------------

absolute_value(Value, NewValue) :-
    (Value > 0,
    NewValue is Value);
    (Value < 0,
    NewValue is Value * (-1)).

% -------------------------------------------------------------------------------

get_list_of_adjacents(Board, Player, InitialI, InitialJ, ListOfAdjacents):-
    findall([FinalI, FinalJ], (
        nth1(FinalI, Board, I_list),
        nth1(FinalJ, I_list, _Space),
        in_bounds(FinalI, FinalJ),
        check_if_adjacent(InitialI, InitialJ, FinalI, FinalJ),
        check_if_different(InitialI, InitialJ, FinalI, FinalJ),
        occupied_by_opponent(Board, FinalI, FinalJ, Player)
    ), ListOfAdjacents).

% ----------------------------------------------------------------------------------


diagonal_rule_righ_up(Initial,'diagonal_right_up', Board, Player):-
    nth1(1, Initial, I),
    nth1(2, Initial, J),
    occupied_by_opponent(Board, I-1, J, Player),
    occupied_by_opponent(Board, I, J+1, Player).

% ------------------------------------------------------------------------------

define_diagonal_direction([InitialI, InitialJ], [FinalI, FinalJ], Direction):-
    AuxI is FinalI - InitialI,
    AuxJ is FinalJ - InitialJ,
    (AuxI =:= AuxJ, AuxI > 0 -> Direction = 'diagonal_right_up';
    AuxI =:= - AuxJ, AuxI < 0 -> Direction = 'diagonal_right_down';
    AuxI =:= - AuxJ, AuxI > 0 -> Direction = 'diagonal_left_up';
    AuxI =:= AuxJ, AuxI < 0 -> Direction = 'diagonal_left_down';
    Direction = 'not_diagonal').

% ------------------------------------------------------------------------------------------

diagonal_rule_right_up(Initial,'diagonal_right_up', Board, Player):-
    nth1(1, Initial, I),
    nth1(2, Initial, J),
    NewI is I-1,
    occupied_by_opponent(Board, NewI, J, Player),
    NewJ is J+1,
    occupied_by_opponent(Board, I, NewJ, Player).

diagonal_rule_right_down(Initial,'diagonal_right_down', Board, Player):-
    nth1(1, Initial, I),
    nth1(2, Initial, J),
    NewJ is J+1,
    occupied_by_opponent(Board, I, NewJ, Player),
    NewI is I+1,
    occupied_by_opponent(Board, NewI, J, Player).

diagonal_rule_left_up(Initial,'diagonal_left_up', Board, Player):-
    nth1(1, Initial, I),
    nth1(2, Initial, J),
    NewI is I-1,
    occupied_by_opponent(Board, NewI, J, Player),
    NewJ is J-1,
    occupied_by_opponent(Board, I, NewJ, Player).

diagonal_rule_left_down(Initial,'diagonal_left_down', Board, Player):-
    nth1(1, Initial, I),
    nth1(2, Initial, J),
    NewJ is J-1,
    occupied_by_opponent(Board, I, NewJ, Player),
    NewI is I+1,
    occupied_by_opponent(Board, NewI, J, Player).

% --------------------------------------------------------------------------------------------------

diagonal_rules_directions(Initial, Direction, Board, Player):-
    diagonal_rule_right_up(Initial,'diagonal_right_up', Board, Player);
    diagonal_rule_right_down(Initial,'diagonal_right_down', Board, Player);
    diagonal_rule_left_up(Initial,'diagonal_left_up', Board, Player);
    diagonal_rule_left_down(Initial,'diagonal_left_down', Board, Player).

% ---------------------------------------------------------------------------------------------------

diagonal_rules(Initial, Final, Board, Player):-
    define_diagonal_direction(Initial, Final, Direction),
    \+ diagonal_rules_directions(Initial, Direction, Board, Player).

% -------------------------------------------------------------------------------------------------

distance(I1, J1, I2, J2, Distance):-
	A is I1 - I2,
	B is J1 - J2,
	AA is A * A,
	BB is B * B,
	AB is AA + BB,
	Distance is floor(sqrt(AB)).    