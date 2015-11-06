% -------------- Winning conditions

% Completed islands

completed_island(Board) :- completed_island('white', Board).
completed_island(Board) :- completed_island('black', Board).
completed_island(Board) :- completed_island('round', Board).
completed_island(Board) :- completed_island('square', Board).

completed_island('white', Board) :-
  get_valid_tile(X, Y, Board, [_,1]), !,
  check_white_island(X, Y, Board, Island), !,
  get_list_size(Island, NIsland),
  number_of_tiles(Board, [_,1], N), number_of_tiles(Board, [_,2], N1), NWhites is N+N1, !,
  NIsland =:= NWhites.

completed_island('black', Board) :-
  get_valid_tile(X, Y, Board, [_,4]), !,
  check_black_island(X, Y, Board, Island), !,
  get_list_size(Island, NIsland),
  number_of_tiles(Board, [_,3], N), number_of_tiles(Board, [_,4], N1), NWhites is N+N1, !,
  NIsland =:= NWhites.

completed_island('round', Board) :-
  get_valid_tile(X, Y, Board, [_,1]), !,
  check_round_island(X, Y, Board, Island), !,
  get_list_size(Island, NIsland),
  number_of_tiles(Board, [_,1], N), number_of_tiles(Board, [_,3], N1), NWhites is N+N1, !,
  NIsland =:= NWhites.

completed_island('square', Board) :-
  get_valid_tile(X, Y, Board, [_,2]), !,
  check_square_island(X, Y, Board, Island), !,
  get_list_size(Island, NIsland),
  number_of_tiles(Board, [_,2], N), number_of_tiles(Board, [_,4], N1), NWhites is N+N1, !,
  NIsland =:= NWhites.

% -------------- Check if board is connected

check_if_connected(Board) :-
  number_of_tiles(Board, N),
  check_if_connected(Board, N2), !,
  N =:= N2.

check_if_connected(Board, N) :-
  get_valid_tile(X, Y, Board),
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



% ------------- Check islands

% White
check_white_island(X, Y, Board, Island) :-
  search_white_island([[X,Y]], [], Board, [], Island).

search_white_island([], _, _, Island, Island).
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

% Black
check_black_island(X, Y, Board, Island) :-
  search_black_island([[X,Y]], [], Board, [], Island).

search_black_island([], _, _, Island, Island).
search_black_island([CurrTile|T], Visited, Board, Island, F) :-
  member(CurrTile, Visited), !,
  search_black_island(T, Visited, Board, Island, F).
search_black_island([[X,Y]|T], Visited, Board, Island, F) :-
  get_matrix_elem(X, Y, Board, [_|Tile]), (Tile =:= 3 ; Tile =:= 4),
  neighbours(X, Y, Neighbours),
  append(T, Neighbours, NS), append(Visited, [[X,Y]], NV), append(Island, [[X,Y]], NI),
  search_black_island(NS, NV, Board, NI, F).
search_black_island([Tile|T], Visited, Board, Island, F) :-
  search_black_island(T, [Tile|Visited], Board, Island, F).


% Round
check_round_island(X, Y, Board, Island) :-
  search_round_island([[X,Y]], [], Board, [], Island).

search_round_island([], _, _, Island, Island).
search_round_island([CurrTile|T], Visited, Board, Island, F) :-
  member(CurrTile, Visited), !,
  search_round_island(T, Visited, Board, Island, F).
search_round_island([[X,Y]|T], Visited, Board, Island, F) :-
  get_matrix_elem(X, Y, Board, [_|Tile]), (Tile =:= 1 ; Tile =:= 3),
  neighbours(X, Y, Neighbours),
  append(T, Neighbours, NS), append(Visited, [[X,Y]], NV), append(Island, [[X,Y]], NI),
  search_round_island(NS, NV, Board, NI, F).
search_round_island([Tile|T], Visited, Board, Island, F) :-
  search_round_island(T, [Tile|Visited], Board, Island, F).

% Square
check_square_island(X, Y, Board, Island) :-
  search_square_island([[X,Y]], [], Board, [], Island).

search_square_island([], _, _, Island, Island).
search_square_island([CurrTile|T], Visited, Board, Island, F) :-
  member(CurrTile, Visited), !,
  search_square_island(T, Visited, Board, Island, F).
search_square_island([[X,Y]|T], Visited, Board, Island, F) :-
  get_matrix_elem(X, Y, Board, [_|Tile]), (Tile =:= 2 ; Tile =:= 4),
  neighbours(X, Y, Neighbours),
  append(T, Neighbours, NS), append(Visited, [[X,Y]], NV), append(Island, [[X,Y]], NI),
  search_square_island(NS, NV, Board, NI, F).
search_square_island([Tile|T], Visited, Board, Island, F) :-
  search_square_island(T, [Tile|Visited], Board, Island, F).

% --------------------------- End check islands

% ------------------------- Utils

checkFreeEdges([X,Y], Board, N) :-
  neighbours(X, Y, Neighbours),
  checkFreeEdges(Neighbours, Board, [], ValidTiles),
  get_list_size(ValidTiles, N).

checkFreeEdges([], _, Final, Final).
checkFreeEdges([[X,Y]|T], Board, Valid, F) :-
  get_tile(X,Y,Board,Tile),
  Tile \= 0,
  checkFreeEdges(T, Board, [[X,Y]|Valid], F).
checkFreeEdges([_|T], Board, Valid, F) :- checkFreeEdges(T, Board, Valid, F).


neighbours(X, Y, [N, S, E, W]) :-
  NY is Y - 1, SY is Y + 1, EX is X + 1, WX is X -1,
  N = [X, NY], S = [X, SY], E = [EX, Y], W = [WX, Y].

get_valid_tile(X, Y, Board) :- get_valid_tile(0, 0, X, Y, Board, _).
get_valid_tile(X, Y, Board, Tile) :- get_valid_tile(0, 0, X, Y, Board, Tile).

get_valid_tile(_, _, _, _, [], _).
get_valid_tile(X, Y, X1, Y1, [H|T], Tile) :-
  get_valid_tile_line(X, Y, X1, Y1, H, Tile),
  YN is Y + 1,
  get_valid_tile(X, YN, X1, Y1, T, Tile).

get_valid_tile_line(_, _, _, _, [], _).
get_valid_tile_line(X, Y, X1, Y1, [Curr|_], Tile) :-
 Curr \= [_,0],
 \+ Curr \= Tile,
 X1 = X, Y1 = Y.
get_valid_tile_line(X, Y, X1, Y1, [_|T], Tile) :-
  XN is X + 1,
  get_valid_tile_line(XN, Y, X1, Y1, T, Tile).