createPvPgame(Game, 'major') :-
  createMajorBoard(Board),
  createPlayer(Player1, 'white'), createPlayer(Player2, 'black'),
  Game = [Board, 'major', [Player1, Player2], 'white'].

% Gets
game_getBoard([Board|_], Board).
game_getTurn([_,_,_,Turn], Turn).
game_getPlayers([_,_,Players|_], Players).
game_getMode([_,Mode|_], Mode).

game_setBoard([_|Game], Board, [Board|Game]).
game_setPlayers([Board, Mode, _|Game], Players, [Board, Mode, Players|Game]).

game_nextTurn([Board, Mode, Players, Turn], [Board, Mode, Players, NewTurn]) :-
  switchTurns(Turn, NewTurn).
game_showBoard([Board|_]) :- display_board(Board).

game_whitePass([Board, Mode, [WhitePlayer,BlackPlayer], Turn], [Board, Mode, NewPlayers, Turn]) :-
  player_pass(WhitePlayer, NewWhite),
  NewPlayers = [NewWhite,BlackPlayer].
game_blackPass([Board, Mode, [WhitePlayer,BlackPlayer], Turn], [Board, Mode, NewPlayers, Turn]) :-
  player_pass(BlackPlayer, NewBlack),
  NewPlayers = [WhitePlayer,NewBlack].

game_pass(Game, NewGame) :- game_getTurn(Game, Turn), game_pass(Game, Turn, NewGame).
game_pass(Game, 'white', NewGame) :-
  game_whitePass(Game, Temp), game_nextTurn(Temp, NewGame).
game_pass(Game, 'black', NewGame) :-
  game_blackPass(Game, Temp), game_nextTurn(Temp, NewGame).

game_sink(Game, NewGame) :- game_getTurn(Game, Turn), game_sink(Game, Turn, NewGame).
% game_sink(Game, 'white', NewGame) :-
%   game_whiteSink(Game, Temp).

game_clearPasses(Game, NewGame) :- game_getTurn(Game, Turn), game_clearPasses(Game, Turn, NewGame).
game_clearPasses(Game, 'white', NewGame):-
  game_getPlayers(Game, [W,B]),
  player_clearPasses(W, NW),
  game_setPlayers(Game, [NW,B], NewGame).
game_clearPasses(Game, 'black', NewGame):-
  game_getPlayers(Game, [W,B]),
  player_clearPasses(B, NB),
  game_setPlayers(Game, [W,NB], NewGame).

game_checkPasses([_,_,[W,B]|_], NewGame) :-
  player_getPasses(W, WP), player_getPasses(B, BP),
  WP > 0, BP > 0,
  player_getSinks(W, WS), player_getSinks(B, BS),
  (
    WS > BS -> NewGame = 'White';
    BS > WS -> NewGame = 'Black';
    NewGame = 'White'
  ).
game_checkPasses([_,_,[W,B]|_], NewGame) :-
  player_getPasses(W, WP), player_getPasses(B, BP),
  (
    WP >= 4 -> NewGame = 'Black';
    BP >= 4 -> NewGame = 'White';
    fail
  ).
game_checkPasses(Game, Game).

game_printPlayers([_,_,[W,B]|_]) :-
  player_print(W),
  player_print(B).

switchTurns('white', 'black').
switchTurns('black', 'white').

createPlayer(Player, Color) :-
  Player = ['human', Color, 0, 0].

player_getPasses([_,_,Passes|_], Passes).
player_getSinks([_,_,_,Sinks], Sinks).
player_pass([Type, Color, Passes, Sinks], [Type, Color, NewPasses, Sinks]) :- NewPasses is Passes + 1.
player_clearPasses([Type, Color, _, Sinks], [Type, Color, 0, Sinks]).

player_print([Type, Color, Passes, Sinks]) :-
  (
    Color == 'white' -> C = 'White';
    Color == 'black' -> C = 'Black'
  ),
  (
    Type == 'human' -> T = 'Human';
    Type == 'pc'    -> T = 'PC'
  ),
  write(C), write(' - '), write(T), write(' | '), write(Passes), write(' Passes | '),
  write(Sinks), write(' Sinks'), nl.

createMajorBoard(Board) :-
  gen_major(Temp),
  completed_island(Temp),
  createMajorBoard(Board).
createMajorBoard(Board) :-
  gen_major(Temp),
  Board = Temp.