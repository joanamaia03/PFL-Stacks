:- consult(inputs).

% main menu where I can access the different player mode options
menu:-
    write('***************************'),nl,
    write('*          Menu           *'),nl,
    write('***************************'),nl,
    write('* 1- Human vs Human       *'),nl,
    write('* 2- Human vs Computer    *'),nl,
    write('* 3- Computer vs Computer *'),nl,
    write('* 4- Exit                 *'),nl,
    write('***************************'),nl,
    
    menu_option(1,4,'Select game mode',N),
    Option is N,
    game_option(Option).

game_option(1):-
    
% secundary menu where I can access the differente difficulty leves for the computer player
game_option(2):-
    write('***********************'),nl,
    write('*     Difficulty      *'),nl,
    write('***********************'),nl,
    write('* 1- Easy             *'),nl,
    write('* 2- Greedy           *'),nl,
    write('***********************'),nl,

    menu_option(1,2,'Select difficulty',N).

game_option(3):-
    write('***********************'),nl,
    write('*     Difficulty      *'),nl,
    write('***********************'),nl,
    write('* 1- Easy             *'),nl,
    write('* 2- Greedy           *'),nl,
    write('***********************'),nl,

    menu_option(1,2,'Select difficulty',N).

game_option(4):-
    abort.
