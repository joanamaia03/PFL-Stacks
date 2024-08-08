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