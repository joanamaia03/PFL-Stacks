:- use_module(library(lists)).

% valid_moves(+GameState, +Player, -ListOfMoves). 
% GameState = [Board, PiecesIG1, PiecesRM1, PiecesIG2, PiecesRM2, Turn, CountNeg, CountPos].
% Turn can be 1 or 2, depending on what player should play.
valid_moves(GameState, ListOfMoves):-
    nth1(6, GameState, Player),
    nth1(1, GameState, Board),
    find_players_pieces(Board,Player,Pieces),
    get_list_of_moves(Board, Player, Pieces, ListOfMoves).

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

% Accessing a value in the board.
% I is the index for the rows and J for the columns.
matrix(Board, I, J, Value):-
    nth1(I, Board, Row),
    nth1(J, Row, Value).

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

in_bounds(I, J) :-
    I < 8,
    J < 6,
    I > 0,
    J > 0.

check_if_adjacent(I1,J1,I2,J2):-
    (I1=:=I2, abs(J1-J2)=:=1);
    (J1 =:= J2, abs(I1 - I2) =:= 1);
    (abs(I1-I2)=:=1, abs(J1-J2)=:=1). 

check_if_different(I1,J1,I2,J2):-
    \+ [I1,J1] = [I2,J2]. 

 occupied_by_opponent(Board, I, J, Player) :-
    (Player = 1,
    matrix(Board, I, J, Value),
    Value < 0);
    (Player = 2,
    matrix(Board, I, J, Value),
    Value > 0).  

% Checks if it can go to a friendly space (may be occupied by a space that is not occupied or that is occupied by a friendly space).
% As a stack cannot be bigger than 4, checks if the sum is less than 5.
% Both numbers must be in its absolute value.
can_friendly(ValueMover, ValueThere) :-
    (ValueMover + ValueThere) < 5.



% base case where the allowance is 0.
% check_allowance(initialI, initialJ, I, J, allowance) where initialI==I and initialJ==J.
check_allowance(_Board, 0):- !.

% check_allowance(initialI, initialJ, allowance).
check_allowance(Board, Allowance, FinalI, FinalJ):-
    Allowance > 0,
    NewAllowance is Allowance - 1,
    append(NewBoard,[],Board),
    find_players_pieces(Board,Player,Pieces),
    get_list_of_moves(NewBoard, Player, Pieces, ListOfMoves),
    nth1(1, Pieces, I),
    nth1(2, Pieces, J),
    replace_matrix(0, I, J, Pieces, NewBoard),
    nth1(1, Final, FianlI),
    nth1(2, Final, FinalJ),
    NewValue is , % é suposto ser o valor antigo + o novo mas ns como é
    replace_matrix( NewValue, FinalI, FinalJ, Final, NewBoard),
    check_allowance( NewBoard, NewAllowance, FinalI, FinalJ).

absolute_value(Value, NewValue) :-
    (Value > 0,
    NewValue is Value);
    (Value < 0,
    NewValue is Value * (-1)).

% replace element on line 1 and a specific column
replace_matrix(New, 1, X,[R|Rows], [Result|Rows]):-
    replace_element(New, X, R, Result).
% replace element on a specific line and column recursively
replace_matrix(New, Y, X, [R|Rows], [R|Result]):-
    Y>1,
    Y1 is Y-1,
    replace_matrix(New, Y1, X, Rows, Result).

% replace the first element on column 1
replace_element(New, 1, [_|Tail], [New|Tail]).
% replace element on column X recursively
replace_element(New, X, [H|Tail],[H|Result]):-
    X>1,
    X1 is X-1,
    replace_element(New,X1,Tail,Result).         

distance(I1, J1, I2, J2, Distance):-
	A is I1 - I2,
	B is J1 - J2,
	AA is A * A,
	BB is B * B,
	AB is AA + BB,
	Distance is floor(sqrt(AB)).    