mem(X, []) :- fail.
mem(X, [X|_]) :- !.
mem(X, [_|Ra]) :- mem(X, Ra).