:- use_module(library(lists)).

get_list_of_adjacents(Board, InitialI, InitialJ, ListOfAdjacents):-
    findall([FinalI, FinalJ], (
        nth1(FinalI, Board, I_list),
        nth1(FinalJ, I_list, _Space),
        in_bounds(FinalI, FinalJ),
        check_if_adjacent(InitialI, InitialJ, FinalI, FinalJ),
        check_if_different(InitialI, InitialJ, FinalI, FinalJ)
    ), ListOfAdjacents).

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

check_cannot_move(Board, I, J, Player) :-
    get_list_of_adjacents(Board, I, J, Adjacents),
    % iterar pela lista e ver se é ocupada pelo oponente
    nth1(_, Adjacents, Adj),
    Adj = [AdjI, AdjJ],
    occupied_by_opponent(Board, AdjI, AdjJ, Player).           

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