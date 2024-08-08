% To confirm if the piece that is going to move correspond to a piece from the player that is trying to move it.
is_player1_piece(Board, I, J, 1):-
    matrix(Board, I, J, Value),
    Value > 0.
is_player2_piece(Board, I, J, 2):-
    matrix(Board, I, J, Value),
    Value < 0.

find_player1_pieces(Board, Pieces):-
    findall((I, J), is_player1_piece(Board, I, J, 1), Pieces). 

find_player2_pieces(Board, Pieces):-
    findall((I, J), is_player2_piece(Board, I, J, 2), Pieces). 