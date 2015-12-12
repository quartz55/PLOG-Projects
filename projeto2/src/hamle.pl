:- use_module(library(clpfd)).

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
% | |2| |    |1| | |
% |1| | | -> | | | |
% | | | |    | |2| |
%%% ----------

easy_puzzle([
             [0,2,0],
             [1,0,0],
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
  easy_puzzle(P),
  display_board(P),
  write('----------------'), nl,

  !, solve_puzzle(P, Sol, Pieces, Length)

  ,display_solution_board(Sol, Pieces, Length)
  .

display_solution_board(Sol, Pieces, Length) :-
  length(Aux, Length),
  exactly(0, Aux, Length),
  Size is integer(sqrt(Length)),
  put_pieces(Sol, Pieces, Aux, Final), !,
  list_to_matrix(Final, Size, M),
  display_board(M).

put_pieces([], [], Final, Final).
put_pieces([Pos|T], [[_,Piece]|PT], Aux, Final) :-
  Pos1 is Pos - 1,
  replace(Aux, Pos1, Piece, Aux1),
  put_pieces(T, PT, Aux1, Final).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

%%%%%%%%%%
% Solver %
%%%%%%%%%%

solve_puzzle(Board, Solution, PiecesList, Length) :-
  get_board_size(Board, Size),
  Length #= Size * Size,
  get_board_pieces(Board, PiecesList),
  length(PiecesList, NumPieces),

  write('Pieces List: '), write(PiecesList), nl,
  format('Num Pieces: ~d\n', NumPieces),
  format('Domain: [~d - ~d]\n', [1, Length]),

  length(Solution, NumPieces),
  domain(Solution, 1, Length),
  all_different(Solution),

  !,

  %% 1st restriction
  %%%% Checks all the possible cells a piece can move to based on its number
  check_piece_moves(Board, PiecesList, Solution),

  write(Solution), nl,
  %% 2nd restriction
  %%%% Checks if pieces don't have adjacent pieces to them
  check_piece_adjacency(Board, Solution, Solution),
  write(Solution), nl,

  %% Make sure pieces position corresponds to movement allowed
  % check_pieces_moves(Board, Solution),

  % check_interconnection(Solution),

  % check_no_adjacency(Solution),

  !, labeling([ff], Solution).

check_piece_adjacency(_, [], _).
check_piece_adjacency(Board, [Pos|T], Solution) :-
  get_board_size(Board, Size),
  Pos1 #= Pos - 1,
  convert_1d_to_2d(Pos1, Size, Pos2D),
  get_valid_neighbours(Pos2D, 1, Board, Adjacents),
  validate_adjacency(Adjacents, Solution),
  check_piece_adjacency(Board, T, Solution).

validate_adjacency([], _).
validate_adjacency([Val|T], Solution) :-
  count(Val, Solution, #=, 0),
  validate_adjacency(T, Solution).

check_piece_moves(_, [], []).
check_piece_moves(Board, [Piece|T], [X|XT]) :-
  move_piece(Piece, Board, X),
  check_piece_moves(Board, T, XT).

move_piece([Pos, Num], Board, Var) :-
  get_valid_neighbours(Pos, Num, Board, Neighbours),
  set_piece_movements(Neighbours, Var).

set_piece_movements([Pos1], Var) :-
  Var #= Pos1.
set_piece_movements([Pos1, Pos2], Var) :-
  Var #= Pos1 #\/ Var #= Pos2.
set_piece_movements([Pos1, Pos2, Pos3], Var) :-
  Var #= Pos1 #\/ Var #= Pos2 #\/ Var #= Pos3.
set_piece_movements([Pos1, Pos2, Pos3, Pos4], Var) :-
  Var #= Pos1 #\/ Var #= Pos2 #\/ Var #= Pos3 #\/ Var #= Pos4.

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
  X is Pos mod Size,
  Y is Pos // Size.
convert_2d_to_1d([X,Y], Size, Result) :-
  Result is (Y * Size) + X.

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
  convert_2d_to_1d(H, Size, Temp), Conv #= Temp + 1,
  convert_to_1d(T, Size, [Conv|Acc], Valid).

get_neighbours([X,Y], [N,S,E,W], Distance) :-
  NY is Y - Distance,
  SY is Y + Distance,
  EX is X + Distance,
  WX is X - Distance,
  N = [X, NY], S = [X, SY], E = [EX, Y], W = [WX, Y].

%%% Board must be square
get_board_size([Line|XT], Size) :-
  length(Line, Width), length([Line|XT], Height),
  Width = Height, Size = Width.
get_board_size(_,_) :- write('Board is not a square'), !, fail.


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
exactly(_,[],0).
exactly(X,[Y|T],N) :-
  X #= Y #<=> B,
  N #= M+B,
  exactly(X,T,M).