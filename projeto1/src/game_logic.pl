% Check if board is connected

check_if_connected(Board) :-
  number_of_tiles(Board, N),
  check_if_connected(Board, N2),
  N =:= N2.

check_if_connected(Board, N) :-
  get_valid_tile(0, 0, X, Y, Board),
  reachable_tiles(X, Y, Board, Reachable), !,
  get_list_size(Reachable, N).

reachable_tiles(X, Y, Board, Reachable) :-
  reachable_tiles_rec([[X,Y]], [], Board, [], Reachable).

reachable_tiles_rec([], _, _, Final, Final).
reachable_tiles_rec([CurrTile|T], Visited, Board, Reachable, F) :-
  member(CurrTile, Visited), !,
  reachable_tiles_rec(T, Visited, Board, Reachable, F).
reachable_tiles_rec([[X,Y]|T], Visited, Board, Reachable, F) :-
  get_matrix_elem(X, Y, Board, CurrTile),
  CurrTile \= [_,0],
  neighbours(X, Y, Neighbours),
  append(T, Neighbours, NT), append(Visited, [[X,Y]], NV), append(Reachable, [[X,Y]], NR),
  reachable_tiles_rec(NT, NV, Board, NR, F).
reachable_tiles_rec([CurrTile|T], Visited, Board, Reachable, F) :-
  reachable_tiles_rec(T, [CurrTile|Visited], Board, Reachable, F).



% Check islands

check_white_island(X, Y, Board, Island) :-
  search_white_island([[X,Y]], [], Board, [], Island).

search_white_island([], _, _, Island, Island) :- write(Island), nl.
search_white_island([CurrTile|T], Visited, Board, Island, F) :-
  member(CurrTile, Visited), !,
  search_white_island(T, Visited, Board, Island, F).
search_white_island([[X,Y]|T], Visited, Board, Island, F) :-
  get_matrix_elem(X, Y, Board, [_|Tile]), (Tile =:= 1 ; Tile =:= 2),
  neighbours(X, Y, Neighbours),
  append(T, Neighbours, NS), append(Visited, [[X,Y]], NV), append(Island, [[X,Y]], NI),
  search_white_island(NS, NV, Board, NI, F).
search_white_island([Tile|T], Visited, Board, Island, F) :-
  search_white_island(T, [Tile|Visited], Board, Island, F).

% Utils

neighbours(X, Y, [N, S, E, W]) :-
  NY is Y - 1, SY is Y + 1, EX is X + 1, WX is X -1,
  N = [X, NY], S = [X, SY], E = [EX, Y], W = [WX, Y].

get_valid_tile(_, _, _, _, []).
get_valid_tile(X, Y, X1, Y1, [H|T]) :-
  get_valid_tile_line(X, Y, X1, Y1, H),
  YN is Y + 1,
  get_valid_tile(X, YN, X1, Y1, T).

get_valid_tile_line(_, _, _, _, []).
get_valid_tile_line(X, Y, X1, Y1, [[_,Tile]|_]) :- Tile \= 0, X1 = X, Y1 = Y.
get_valid_tile_line(X, Y, X1, Y1, [_|T]) :-
  XN is X + 1,
  get_valid_tile_line(XN, Y, X1, Y1, T).