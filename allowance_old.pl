
check_if_adjacent(I1,J1,I2,J2):-
    (I1=:=I2, abs(J1-J2)=:=1);
    (J1 =:= J2, abs(I1 - I2) =:= 1);
    (abs(I1-I2)=:=1, abs(J1-J2)=:=1).


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

% We give the initial coordenates and it returns the final ones
move_directions(InitialI, InitialJ, FinalI, FinalJ):-
    move_up(InitialI, InitialJ, FinalI, FinalJ),
    move_down(InitialI, InitialJ, FinalI, FinalJ);
    move_left(InitialI, InitialJ, FinalI, FinalJ);
    move_right(InitialI, InitialJ, FinalI, FinalJ);
    move_diagonal_left_up(InitialI, InitialJ, FinalI, FinalJ);
    move_diagonal_left_down(InitialI, InitialJ, FinalI, FinalJ);
    move_diagonal_right_up(InitialI, InitialJ, FinalI, FinalJ);
    move_diagonal_right_down(InitialI, InitialJ, FinalI, FinalJ).     


% base case where the allowance is 0.
% move_allowance(initialI, initialJ, I, J, allowance) where initialI==I and initialJ==J.
move_allowance(InitialI, InitialJ, FinalI, FinalJ, 0):- !.

% move_allowance(initialI, initialJ, I, J, allowance).
move_allowance( InitialI, InitialJ, FinalI, FinalJ, Allowance):-
    Allowance > 0,
    check_if_adjacent(InitialI, InitialJ, FinalI, FinalJ),
    NewAllowance is Allowance - 1,
    move_allowance( I1, J1, If, Jf, NewAllowance).


% save the coordenates of the final position on the list of moves.
% infelizmente nao esta a funcionar matem me
list_position(InitialI, InitialJ, FinalI, FinalJ, ListOfMoves):-
     findall((FinalI, FinalJ), move_directions(InitialI, InitialJ, FinalI, FinalJ), ListOfMoves).  