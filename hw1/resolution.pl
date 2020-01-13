% Name: Hang Zhao  ID:112524698    Computing with Logic


normalize([],[]).
normalize([(_,X)|T],[X1|T1]):-
    flatten(X,X1),
    normalize(T,T1).

append([],L,L).
append([H|T],R,[H|T1]):-
    append(T,R,T1).

flatten(or(X,Y),[X,Y]):-
    X \= or(_,_),
    Y \= or(_,_),
    !.

flatten(X,[X]):-
    X \= or(_,_).
flatten(or(X,Y),R):-
    !,
    flatten(X,R1),
    flatten(Y,R2),
    append(R1,R2,R).

map(_, [], []).
map(F, [A|As], [B|Bs]):-
    call(F, A, B),
    map(F, As, Bs).

map(_,[]).
map(F,[H|T]):-
	call(F,H),
    map(F,T).

len([],0).
len([_|T],N):-
    len(T,N1),
    N is N1 + 1.

member(X,[X|_]).
member(X,[_|T]):-
    member(X,T).

delete(X,[X|T],T).
delete(X,[X1|T],[X1|R]):-
    delete(X,T,R).

different(X,Y):-
    X \= Y.

reverse([],[]).
reverse([H|T], X) :-
	reverse(T,X2),
	append(X2, [H], X).



addQ(Clauses,S1):-
    reverse(Clauses,T),
    myQuery(N,X),
    X \= neg(_),
    !,
    S = [(N,neg(X))|T],
    reverse(S,S1).
    
addQ(Clauses,S1):-
    reverse(Clauses,T),
    myQuery(N,neg(X)),
    S = [(N,X)|T],
    reverse(S,S1).
    
	
resolution :-
    findall((N,C), myClause(N,C), Clauses),
    addQ(Clauses,Clauses1),
	resolution(Clauses1, Rs) 
    	-> 
    		writeln(resolution(success)),
    		prettyWrite(Rs)
    	;   writeln(resolution(fail)).

prettyWrite([(A,B,C,D)|T]):-
    writeln(resolution(A,B,C,D)),
    prettyWrite(T).

resolution(Clauses1, L) :-
    	normalize(Clauses1,Clauses0),
        map(sort, Clauses0, Clauses),
        get_clause(Chain, Clauses),
    	get_list(Clauses1,L1),
    	get_number(Chain,L1,L2),
    	return(L2,L).

get_clause([], Clauses) :-
        member([], Clauses).

get_clause([C|Cs], Clauses) :-
        resolusion_helper(C, Clauses, Rs),
        get_clause(Cs, [Rs|Clauses]).

resolusion_helper((As0,Bs0,Rs), Clauses, Rs) :-
        member(As0, Clauses),
    	member(Bs0, Clauses),
    	As0 \= Bs0,
    	delete(Q, As0, As),
        delete(neg(Q), Bs0, Bs),
        append(As, Bs, Rs0),
        sort(Rs0, Rs),
        \+ member(Rs, Clauses),
        map(different(Rs), Clauses),
    	!.

get_number_helper(L,[(N,L)|_],N).
get_number_helper(L,[(_,L1)|T],N):-
    L \= L1,
    get_number_helper(L,T,N).

get_number([],_,[]).
get_number([(H1,H2,H3)|T],L,[(N1,N2,H3,N4)|T1]):-
    get_largest_number(L,N),
    N4 is N + 1,
    get_number_helper(H1,L,N1),
    get_number_helper(H2,L,N2),
    get_number(T,[(N4,H3)|L],T1).
    
get_list([],[]).
get_list([(N,X)|T],[(N,X2)|T1]):- % (N,X1)
    flatten(X,X1),
    sort(X1,X2),
    get_list(T,T1).

get_largest_number([(N,_)|T],Max):-
    get_largest_number_helper(T,N,Max).
get_largest_number_helper([],Max,Max).
get_largest_number_helper([(N,_)|T],N1,Max):-
    (   N < N1
    ->  get_largest_number_helper(T,N1,Max);
    get_largest_number_helper(T,N,Max)).

return([],[]).
return([(L1,L2,L3,L4)|T],[(L1,L2,L5,L4)|R]):-
    return_helper(L3,L5),
    return(T,R).
return_helper([],empty):-!.
return_helper([X],X).
return_helper([X,Y],or(X,Y)).
return_helper([H1,H2|T],R):-
    X1 = or(H1,H2),
    return_helper([X1|T],R).
