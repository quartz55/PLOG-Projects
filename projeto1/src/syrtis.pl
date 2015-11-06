:- use_module(library(random)).
:- include('utils.pl').
:- include('board.pl').
:- include('game_logic.pl').
:- include('game_class.pl').

syrtis :- start_game.

start_game :- write('Size of board (major/minor)? '), read(X), start_game(X).
start_game('major') :- createPvPgame(Game, 'major'), place_towers(Game).
start_game('minor') :- write('Minor mode is not supported yet. :('), nl, start_game.
start_game(X) :- write(X), write(' not a valid command.'), nl, start_game.

place_towers(Game) :-
  game_getBoard(Game, Board),
  first_tower(Board, B1),
  second_tower(B1, B2),
  third_tower(B2, B3),
  fourth_tower(B3, B4),
  game_setBoard(Game, B4, NewGame),
  game_loop(NewGame).

game_loop('quit') :- !, write('Quitting...'),nl.
game_loop('White') :- write('White won!'),nl.
game_loop('Black') :- write('Black won!'),nl.
game_loop(Game) :-
  game_showBoard(Game),
  game_printPlayers(Game),
  game_getTurn(Game, Turn),
  write('--------------------------'), nl,
  write('It is '), write(Turn), write('\'s turn'), nl,
  write('--------------------------'), nl,
  gameMenu(Option),
  (
    Option = 1 -> moveTowerMenu(Game, NewGame);
    Option = 2 -> sinkTileMenu(Game, NewGame);
    Option = 3 -> moveTileMenu(Game, NewGame);
    Option = 4 -> pass(Game, NewGame);
    Option = 5 -> game_loop('quit');

    nl,
    write('Error: invalid option.'), nl,
    game_loop(Game)
  ),
  game_loop(NewGame).

gameMenu(Option):-
  write('1. Move tower'), nl,
  write('2. Sink tile'), nl,
  write('3. Move tile'), nl,
  write('4. Pass'), nl,
  write('5. Quit'), nl,
  write('> '),
  read(Option).

% ------------- Move tower

moveTowerMenu(Game, NewGame) :-
  game_getTurn(Game, Turn),
  (
    Turn = 'white' -> T = 2;
    Turn = 'black' -> T = 1
    ),
  game_getBoard(Game, Board),
  write('Which tower you want to move?'), nl,
  get_coords(X, Y),
  get_tower(X, Y, Board, Tower), Tower =:= T,
  write('Where do you want to move the tower to?'), nl,
  get_coords(X2, Y2),
  get_tower(X2, Y2, Board, Dest), Dest =:= 0,
  getCurrIsland(X, Y, T, Board, CurrIsland),
  member([X2,Y2], CurrIsland),
  moveTower(X,Y,X2,Y2,T,Board,NewBoard),
  game_setBoard(Game, NewBoard, G1),
  game_clearPasses(G1, G2),
  game_nextTurn(G2, NewGame).
moveTowerMenu(Game, Game) :- write('Invalid move'), nl.


moveTower(X,Y,X2,Y2,Tower,Board,NewBoard) :-
  set_tower(X,Y,Board,0,N),
  set_tower(X2,Y2,N,Tower,NewBoard).
moveTower(_,_,_,_,_,Board,Board) :- write('Couldnt move tower'), nl.

% ------------- Sink tile

sinkTileMenu(Game, Game) :- write('Not implemented yet.'), nl.

% ------------- Move tile

moveTileMenu(Game, Game) :- write('Not implemented yet.'), nl.

% ------------- Pass

pass(Game, NewGame) :-
  game_pass(Game, G1),
  game_checkPasses(G1, NewGame).

% Utils

getCurrIsland(X, Y, 1, Board, Island) :-
  check_black_island(X, Y, Board, I1),
  check_square_island(X, Y, Board, I2),
  append(I1, I2, Island).
getCurrIsland(X, Y, 2, Board, Island) :-
  check_white_island(X, Y, Board, I1),
  check_round_island(X, Y, Board, I2),
  append(I1, I2, Island).

get_coords(X, Y) :-
  write('X coord : '), read(X),
  write('Y coord : '), read(Y).

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
