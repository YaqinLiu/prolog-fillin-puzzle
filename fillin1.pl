:- ensure_loaded(library(clpfd)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).


samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).


% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.

solve_puzzle(Puzzle0, WordList0, Puzzle) :-
    fill(Puzzle0, WordList0, Puzzle1, WordList1),
	transpose(Puzzle1, Puzzle2),
	fill(Puzzle2, WordList1, Puzzle3, []),
	transpose(Puzzle3, Puzzle).

fill([], WordList, [], WordList).	
fill([A|As], WordList0, [B|Bs], WordList1) :-
    fill_line(A, WordList0, B, NewWordList),
	fill(As, NewWordList, Bs, WordList1).

fill_line([], WordList, [], WordList).
fill_line(['#'|As], WordList0, B, WordList1) :-
    fill_line(As, WordList0, Bs, WordList1),
	B = ['#'|Bs].
fill_line(A, WordList0, B, WordList1) :-
    find_sharp(A, N),
	length(Slot, N),
	append(Slot, Rest, A),
	length(Word, N),
	append(Front, [Word|Tail], WordList0),
	match_word(Slot, Word),
	append(Front, Tail, NewWordList),
	fill_line(Rest, NewWordList, Rest1, WordList1),
	append(Word, Rest1, B).
	
find_sharp([], 0).
find_sharp(['#'|_], 0).
find_sharp([A|As], N) :-
    A \= '#',
    find_sharp(As, N1),
    N is N1 + 1.

match_word([], []).
match_word(['_'|As], [_|Bs]) :-
    match_word(As, Bs).
match_word([X|As], [X|Bs]) :-
    match_word(As, Bs).
    

	
	

    
	



























