fill([], []).
fill([A|As], WordList) :-
    setof(A, member(A, WordList), Words),
	length(Words, N),
	(N < 3 ->
	member(X, Words),
	A = X,
	select(X, WordList, NewWordList),
	fill(As, NewWordList);
	append(As, [A], Slots),
	fill(Slots, WordList)
	).
	
find_shortest([A|[]], A, 1).
find_shortest([A|As], Choice, N) :-
    find_shortest(As, Choice0, N0),
	length(A, X),
	length(Choice0, Y),
	(   X =< Y,
	    Choice = A,
	    N is 1
	;   X >= Y,
	    Choice = Choice0,
	    N is N0 + 1
	).