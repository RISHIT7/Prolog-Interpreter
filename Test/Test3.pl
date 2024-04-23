% Facts
parent(john, jim).
parent(john, ann).
parent(susan, jim).
parent(susan, ann).
male(john).
female(susan).

% Rules
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).