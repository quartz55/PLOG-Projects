writeln([X|Xs]) :- write(X), writeln(Xs).
writeln([]) :- nl.

echo :- read(X), echo(X).
echo(X) :- last_input(X), !.
echo(X) :- write(X), nl, read(Y), !, echo(Y).

last_input('quit').