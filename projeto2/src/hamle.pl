:- use_module(library(clpfd)).
:- use_module(library(lists)).

% Board: 6x6

% | |3| | | |2|    | | | |2| | |
% | | |3| |4| |    |4| | | | |3|
% | |1| | | | | -> | | |1| | | |
% |5| | |2| |2| -> | |3| |2| |5|
% | | | | | | |    | | | | | | |
% | |4| |2| | |    | |2| |2| |4|

base_puzzle([
             [0,3,0,0,0,2],
             [0,0,3,0,4,0],
             [0,1,0,0,0,0],
             [5,0,0,2,0,2],
             [0,0,0,0,0,0],
             [0,4,0,2,0,0]
            ]).

solve_puzzle(Board, Solution).

check_interconnection(Board) :-
  check_interconnection(Board, [], []).

check_white([X,Y], Board).

% Board utils

get_whites(Board, [[X,Y]|XT]) :-
  get_cell([X,Y], Board, 0),
  get_whites(Board, XT).

get_white_connection(Board, List) :-
  get_cell([X,Y], Board, 0),
  get_white_connection([[X,Y]], [], List, Board).

get_cell([X,Y], Board, Cell) :-
  get_board_size(Board, Size), !,
  (X #>= 0, X #< Size), !,
  (Y #>= 0, Y #< Size),
  nth0(Y, Board, Line),
  nth0(X, Line, Cell).

get_board_size([Line|XT], Size) :-
  length(Line, Width), length([Line|XT], Height),
  Width = Height, Size = Width.
get_board_size(_,_) :- write('Board is not a square'), !, fail.

display_board([]).
display_board([Line|XT]) :-
  write('|'), display_line(Line), nl,
  display_board(XT), !.

display_line([]).
display_line([El|XT]) :-
  display_element(El), write('|'),
  display_line(XT), !.

display_element(0) :- write(' ').
display_element(T) :- write(T).