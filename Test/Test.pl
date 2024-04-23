type(intT).
type(boolT).

hastype(intT(N),intT) :- integer(N), !.

hastype(boolT(B), boolT) :- member(B, [true, false]), !.

hastype(plus(E1, E2), intT) :- hastype(E1, intT), hastype(E2, intT).

hastype(times(E1, E2), intT) :- hastype(E1, intT), hastype(E2, intT).

hastype(or(E1, E2), boolT) :- hastype(E1, boolT), hastype(E2, boolT).

hastype(and(E1, E2), boolT) :- hastype(E1, boolT), hastype(E2, boolT).

hastype(not(E1), boolT) :- hastype(E1, boolT).

hastype(eq(E1, E2), boolT) :- hastype(E1, intT), hastype(E2, intT).

hastype(gt(E1, E2), boolT) :- hastype(E1, intT), hastype(E2, intT).
