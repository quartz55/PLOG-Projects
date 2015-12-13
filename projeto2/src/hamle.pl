:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(sets)).
:- use_module(library(random)).

%%%%%%%%%%%%%%%
% Test Boards %
%%%%%%%%%%%%%%%

%%% Board: 6x6
% | |3| | | |2|    | | | |2| | |
% | | |3| |4| |    |4| | | | |3|
% | |1| | | | | -> | | |1| | | |
% |5| | |2| |2| -> | |3| |2| |5|
% | | | | | | |    | | | | | | |
% | |4| |2| | |    | |2| |2| |4|
%%% ----------

base_puzzle([
             [0,3,0,0,0,2],
             [0,0,3,0,4,0],
             [0,1,0,0,0,0],
             [5,0,0,2,0,2],
             [0,0,0,0,0,0],
             [0,4,0,2,0,0]
            ]).

%%% Board: 3x3
% |2| |2|    |2| | |
% | |1| | -> | | |1|
% | | | |    |2| | |
%%% ----------

easy_puzzle([
             [2,0,2],
             [0,1,0],
             [0,0,0]
            ]).

%%% Board: 2x2
% |1| | -> | | | // | |1|
% | | | -> |1| | // | | |
%%% ----------

test_puzzle([
             [1,0],
             [0,0]
            ]).

%%%%%%%%%
% Hamle %
%%%%%%%%%

hamle(Sol) :-
  base_puzzle(P),
  display_board(P),
  write('----------------'), nl,

  !, solve_puzzle(P, Sol, Pieces, Length),

  display_solution_board(Sol, Pieces, Length),
  fd_statistics.

hamle_random(Size, Sol) :-
  generate_random_board(Size, B),
  write('Generating'), nl,
  solve_puzzle(B, Sol, Pieces, Length),
  display_board(B),
  write('----------------'), nl,
  display_solution_board(Sol, Pieces, Length),
  fd_statistics.
hamle_random(Size, Sol) :- hamle_random(Size, Sol).

display_solution_board(Sol, Pieces, Length) :-
  length(Aux, Length),
  exactly(0, Aux, Length),
  Size is integer(sqrt(Length)),
  put_pieces(Sol, Pieces, Aux, Final), !,
  list_to_matrix(Final, Size, M),
  display_board(M).

put_pieces([], [], Final, Final).
put_pieces([Pos|T], [[_,Piece]|PT], Aux, Final) :-
  replace(Aux, Pos, Piece, Aux1),
  put_pieces(T, PT, Aux1, Final).


%%%%%%%%%%
% Solver %
%%%%%%%%%%

solve_puzzle(Board, Solution, PiecesList, Length) :-
  get_board_size(Board, Size),
  Length #= Size * Size,
  get_board_pieces(Board, PiecesList),
  length(PiecesList, NumPieces),

  MaxDomain #= Length - 1,
  length(Solution, NumPieces),
  domain(Solution, 0, MaxDomain),
  all_different(Solution),

  !,

  move_pieces_and_check_adjacency(Board, PiecesList, Solution, Solution),

  %% Need to check white piece connection
  %% ...

  labeling([ff], Solution).

set_adjacent_restriction(_, []).
set_adjacent_restriction([A,B,C,D], [H|T]) :-
  H #\= A #/\
  H #\= B #/\
  H #\= C #/\
  H #\= D,
  set_adjacent_restriction([A,B,C,D], T).
set_adjacent_restriction([A,B,C], [H|T]) :-
  H #\= A #/\
  H #\= B #/\
  H #\= C,
  set_adjacent_restriction([A,B,C], T).
set_adjacent_restriction([A,B], [H|T]) :-
  H #\= A #/\
  H #\= B,
  set_adjacent_restriction([A,B], T).

get_adjacent([N,S,E,W], X, Size) :-
  N #= X - Size #/\
  S #= X + Size #/\
  E #= X + 1 #/\ E mod Size #\= 0 #/\
  W #= X - 1 #/\ W mod Size #< Size - 1.
get_adjacent([N,S,E], X, Size) :-
  N #= X - Size #/\
  S #= X + Size #/\
  E #= X + 1 #/\ E mod Size #\= 0.
get_adjacent([N,S,W], X, Size) :-
  N #= X - Size #/\
  S #= X + Size #/\
  W #= X - 1 #/\ W mod Size #< Size - 1.
get_adjacent([N,S], X, Size) :-
  N #= X - Size #/\
  S #= X + Size.


move_pieces_and_check_adjacency(_, [], [], _).
move_pieces_and_check_adjacency(Board, [Piece|T], [X|XT], Sol) :-
  move_piece(Piece, Board, X),
  get_board_size(Board, Size),
  get_adjacent(List, X, Size),
  % convert_1d_to_2d(X, Size, Pos2D),
  % write('----- Adjacents -----'), nl,
  % write(Pos2D), write(': '),
  % write(List), nl,
  % write(L), nl,
  % write('---------------------'), nl,
  set_adjacent_restriction(List,Sol),
  move_pieces_and_check_adjacency(Board, T, XT, Sol).

move_piece([Pos, Num], Board, Var) :-
  get_valid_neighbours(Pos, Num, Board, Neighbours),
  % write('----- Movement -----'), nl,
  % write(Num), write(' | '), write(Neighbours), nl,
  % write('--------------------'), nl,
  list_to_fdset(Neighbours, Set),
  Var in_set Set.

%%%%%%%%%%%%%%%
% Board utils %
%%%%%%%%%%%%%%%

get_board_pieces(Board, PiecesList) :-
  get_board_size(Board, Size),
  get_board_pieces_aux(Board, [0,0], Size, [], PiecesList).

get_board_pieces_aux(_, [_, Size], Size, Final, Final).
get_board_pieces_aux(Board, [X,Y], Size, Acc, Final) :-
  X < Size, X1 is X + 1,
  get_matrix_elem([X,Y], Board, Elem),
  ((Elem \= 0, append(Acc, [[[X,Y],Elem]], Acc1)) ; Acc1 = Acc),
  get_board_pieces_aux(Board, [X1, Y], Size, Acc1, Final).
get_board_pieces_aux(Board, [_,Y], Size, Acc, Final) :-
  Y1 is Y + 1, get_board_pieces_aux(Board, [0, Y1], Size, Acc, Final).


convert_1d_to_2d(Pos, Size, [X,Y]) :-
  X #= Pos mod Size,
  Y #= Pos // Size.
convert_2d_to_1d([X,Y], Size, Result) :-
  Result #= (Y * Size) + X.

get_valid_neighbours([X,Y], Distance, Board, Valid) :-
  get_board_size(Board, Size),
  get_neighbours([X,Y], Aux, Distance),
  trim_invalid_neighbours(Aux, Board, [], Neighbours),
  convert_to_1d(Neighbours, Size, [], Valid).

trim_invalid_neighbours([], _, Final, Final).
trim_invalid_neighbours([H|T], Board, Acc, Valid) :-
  get_matrix_elem(H, Board, _),
  trim_invalid_neighbours(T, Board, [H|Acc], Valid).
trim_invalid_neighbours([_|T], Acc, Valid, Board) :- trim_invalid_neighbours(T, Acc, Valid, Board).

convert_to_1d([], _, Valid, Valid).
convert_to_1d([H|T], Size, Acc, Valid) :-
  convert_2d_to_1d(H, Size, Conv),
  convert_to_1d(T, Size, [Conv|Acc], Valid).

get_neighbours([X,Y], [[X,NY],[X,SY],[EX,Y],[WX,Y]], Distance) :-
  NY is Y - Distance,
  SY is Y + Distance,
  EX is X + Distance,
  WX is X - Distance.

get_board_size(Board, Size) :-
  length(Board, Size).

generate_random_board(Size, Board) :-
  generate_random_board(Size, Size, [], Board).

generate_random_board(0, _, Board, Board) :- !.
generate_random_board(N, Size, Acc, Board) :-
  N1 is N - 1,
  generate_random_line(Size, Line),
  append(Acc, [Line], Acc1),
  generate_random_board(N1, Size, Acc1, Board).

generate_random_line(Size, Line) :-
  generate_random_line(Size, Size, [], Line).

generate_random_line(0, _, Line, Line) :- !.
generate_random_line(N, Size, Acc, Line) :-
  N1 is N - 1,
  random(0, Size, Rand),
  append(Acc, [Rand], Acc1),
  generate_random_line(N1, Size, Acc1, Line).

%%% Display functions
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


%%%%%%%%%
% Utils %
%%%%%%%%%

%%% List->Matrix / Matrix->List converters
matrix_to_list(List, Matrix) :-
  matrix_to_list(List, [], Matrix).

matrix_to_list([], Final, Final).
matrix_to_list([H|T], M, Final) :-
  append(M, H, M1),
  matrix_to_list(T, M1, Final).

list_to_matrix([], _, []).
list_to_matrix(List, Size, [Row|Matrix]):-
  list_to_matrix_row(List, Size, Row, Tail),
  list_to_matrix(Tail, Size, Matrix).

list_to_matrix_row(Tail, 0, [], Tail).
list_to_matrix_row([Item|List], Size, [Item|Row], Tail):-
  NSize is Size-1,
  list_to_matrix_row(List, NSize, Row, Tail).

%%% Getters
get_matrix_as_list_elem(MatrixAsList, [Width, Height], [X, Y], Elem) :-
  X < Width, Y < Height,
  N is (Y * Width) + X,
  get_list_elem(MatrixAsList, N, Elem).

get_matrix_elem([X,Y], Matrix, Elem) :-
  get_list_elem(Matrix, Y, Line),
  get_list_elem(Line, X, Elem).

get_list_elem([Elem|_], 0, Elem).
get_list_elem([_|T], N, Elem) :-
  N > 0, N1 is N - 1,
  get_list_elem(T, N1, Elem).

%%% Others

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

exactly(_,[],0).
exactly(X,[Y|T],N) :-
  X #= Y #<=> B,
  N #= M+B,
  exactly(X,T,M).