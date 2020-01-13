% Name: Hang Zhao  ID:112524698    Computing with Logic

delete(X,[X|T],T).
delete(X,[X1|T],[X1|R]):-
    delete(X,T,R).


member(X,[X|_]).
member(X,[_|T]):- 
    member(X,T).

superset([],[]). 
superset([_|T],S):-
    superset(T,S). 
superset([X|XS],[X|YS]):-
    superset(XS,YS).

tp_helper([], _, []). 
tp_helper([Rule|T1], XS, R) :-
    copy_term(Rule, rule(X, Y)),
    findall(X, (superset(XS, Y)), Conss),
    tp_helper(T1, XS, R2), 
    combine1(Conss, R2, R).

len([],0). 
len([_|T],N):-
    len(T,N1), N is N1 + 1.

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

transformation([], _, []) :- !.
transformation([Rule|T1], X, [Rule|Q]) :-
    copy_term(Rule, rule(_, Y)),
    not(member(not(_), Y)),!,
    transformation(T1, X, Q).
transformation([Rule|T1], X, Q) :-
    copy_term(Rule, rule(_, Y)),
    member(not(A), Y),
    member(A, X),!,
    transformation(T1, X, Q).
transformation([Rule|T1], X, [rule(H, NY)|Q]) :-
    copy_term(Rule, rule(H, Y)),
    del_neg(Y, NY),
    transformation(T1, X, Q).

del_neg([], []) :- !.
del_neg([not(_)|T], R) :- 
    !,
    del_neg(T, R).
del_neg([H|T], [H|R]) :-
    del_neg(T, R). 

sm(P, X) :-
    transformation(P, X, PS),
    leastmodel(PS, X).

sms(P, L) :-
    propositions(P, Xs),
    findall(X, (superset(Xs, X), sm(P, X)), L).