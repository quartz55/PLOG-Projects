:- use_module(library(clpfd)).

carteiro(N, L, SDist) :-
  length(L, N),
  domain(L, 1, N),
  all_different(L),
  element(N, L, 6),
  circuit(L),
  somadist(L, SDist),
  labeling([maximize(SDist)], L),
  write('Dist: '), write(SDist), nl,
  write('Order: '), write(L).

somadist([_], 0).
somadist([X,Y|R], SDist) :-
  SDist #= abs(X-Y) + SDistRest,
  somadist([Y|R], SDistRest).