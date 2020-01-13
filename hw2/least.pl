% Name: Hang Zhao  ID:112524698    Computing with Logic


member(X,[X|_]).
member(X,[_|T]):-
    member(X,T).

delete(X,[X|T],T).
delete(X,[X1|T],[X1|R]):-
    delete(X,T,R).


superset([],[]).
superset([_|T],S):-
    superset(T,S).
superset([X|XS],[X|YS]):-
    superset(XS,YS).

tp_helper([], _, []).
tp_helper([rule(X,Y)|T1], XS, R) :-
	superset(XS, Y),
	tp_helper(T1, XS, R2),
	combine1([X], R2, R).
tp_helper([rule(_,Y)|T1], XS, R) :-
	\+ superset(XS, Y),
	tp_helper(T1, XS, R).


len([],0).
len([_|T],N):-
    len(T,N1),
    N is N1 + 1.

propositions([], []).
propositions([rule(X,_)|XS], [X|YS]) :-
	propositions(XS, YS).

combine1([], X, X).
combine1([X|Y], L1, [X|R1]) :-
	\+ member(X, L1),
	!,
	combine1(Y, L1, R1).
combine1([_|T1], X, Y) :-
	combine1(T1, X, Y).

tp([], _, []).
tp(X, Y, R) :-
	tp_helper(X, Y, R1),
	sort(R1, R).

leastmodel(X, XS) :-
	least_herbrand_model_helper(X, [], XS).

least_herbrand_model_helper(X, Y, R) :-
	tp(X, Y, S1),
	combine1(Y, S1, S2),
	len(Y, R1),
	len(S2, R2),
	R1 \= R2,
	least_herbrand_model_helper(X, S2, R).

least_herbrand_model_helper(X, R, R) :-
	tp(X, R, S1),
	combine1(R, S1, S2),
	len(R, R1),
	len(S2, R2),
	R1 = R2.
