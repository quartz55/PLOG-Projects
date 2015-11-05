:- use_module(library(random)).
:- include('utils.pl').
:- include('board.pl').

syrtis :- start_game.

start_game :- write('Size of board (major/minor)? '), read(X), start_game(X).
start_game('major') :- gen_major(Board), place_towers(Board).
start_game('minor') :- write('Minor mode is not supported yet. :('), nl, start_game.
start_game(X) :- write(X), write(' not a valid command.'), nl, start_game.

% check_if_connected(Board) :-

place_towers(Board) :-
  first_tower(Board, B1),
  second_tower(B1, B2),
  third_tower(B2, B3),
  fourth_tower(B3, B4),
  display_board(B4).

get_coords(X, Y) :-
  write('X coord : '), read(X1), X is X1 - 1,
  write('Y coord : '), read(Y1), Y is Y1 - 1.

first_tower(Board, NewBoard) :-
  display_board(Board),
  write('Coordinates of first black tower: '), nl,
  get_coords(X, Y),
  check_if_valid_black_tile(X, Y, Board),
  place_tower(X, Y, 1, Board, NewBoard).

second_tower(Board, NewBoard) :-
  display_board(Board),
  write('Coordinates of second black tower: '), nl,
  get_coords(X, Y),
  check_if_valid_black_tile(X, Y, Board),
  place_tower(X, Y, 1, Board, NewBoard).

third_tower(Board, NewBoard) :-
  display_board(Board),
  write('Coordinates of first white tower: '), nl,
  get_coords(X, Y),
  check_if_valid_white_tile(X, Y, Board),
  place_tower(X, Y, 2, Board, NewBoard).

fourth_tower(Board, NewBoard) :-
  display_board(Board),
  write('Coordinates of second white tower: '), nl,
  get_coords(X, Y),
  check_if_valid_white_tile(X, Y, Board),
  place_tower(X, Y, 2, Board, NewBoard).

place_tower(X, Y, Tower, Board, NewBoard) :-
  check_if_no_tower(X, Y, Board),
  set_tower(X, Y, Board, Tower, NewBoard).

check_if_no_tower(X, Y, Board) :-
  get_tower(X, Y, Board, Tower),
  Tower =:= 0 .

check_if_valid_black_tile(X, Y, Board) :-
  get_tile(X, Y, Board, Tile),
  (Tile =:= 2 ; Tile =:= 3 ; Tile =:= 4).

check_if_valid_white_tile(X, Y, Board) :-
  get_tile(X, Y, Board, Tile),
  (Tile =:= 1 ; Tile =:= 2 ; Tile =:= 3).
