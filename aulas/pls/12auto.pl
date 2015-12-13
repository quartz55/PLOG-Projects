:- use_module(library(clpfd)).

autos(L) :-
  Amarelo = 1, Verde = 2, Vermelho = 3, Azul = 4,
  length(L, 12),
  domain(L, 1, 4),
  global_cardinality(L, [Amarelo-4,Verde-2,Vermelho-3,Azul-3]),
  element(1, L, Cor1), element(12, L, Cor12),
  Cor1 #= Cor12,
  element(2, L, Cor2), element(11, L, Cor11),
  Cor2 #= Cor11,
  element(5, L, Cor5), Cor5 #= Azul,
  check_trios(L),
  check_sequence(L, Bs),
  sum(Bs, #=, 1),
  labeling([], L), write(L).

check_trios([X,Y,Z|XT]) :-
  all_different([X,Y,Z]),
  check_trios([Y,Z|XT]).
check_trios([_,_]).

check_sequence([X,Y,Z,W|Xs], [B|Bs]) :-
  Amarelo = 1, Verde = 2, Vermelho = 3, Azul = 4,
  (X #= Amarelo
  #/\
  Y #= Verde
  #/\
  Z #= Vermelho
  #/\
  W #= Azul) #<=> B,
  check_sequence([Y,Z,W|Xs], Bs).
check_sequence([_,_,_], []).