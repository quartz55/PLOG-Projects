:- use_module(library(clpfd)).

puzzle(V) :-
  Amarelo = 1, Azul = 2, Preto = 3, Verde = 4,
  V = [C1, C2, C3, C4, S1, S2, S3, S4],
  domain(V, 1, 4),
  all_different([C1,C2,C3,C4]), all_different([S1,S2,S3,S4]),
  (C2 #= Azul #/\ S1 #< S3) #\/ (C3 #= Azul #/\ S2 #< S4),
  element(PosAzul, [C1,C2,C3,C4], Azul),
  PosAntesAzul #= PosAzul-1,
  PosDepoisAzul #= PosAzul+1,
  element(PosAntesAzul, [S1,S2,S3,S4], TamAntesAzul),
  element(PosDepoisAzul, [S1,S2,S3,S4], TamDepoisAzul),
  TamAntesAzul #< TamDepoisAzul,
  element(PosVerde, [C1,C2,C3,C4], Verde),
  element(PosAmarelo, [C1,C2,C3,C4], Amarelo),
  element(PosPreto, [C1,C2,C3,C4], Preto),
  element(PosVerde, [S1,S2,S3,S4], TamVerde),
  TamVerde #= 1,
  PosVerde #> PosAzul, PosAmarelo #> PosPreto,
  labeling([], V).