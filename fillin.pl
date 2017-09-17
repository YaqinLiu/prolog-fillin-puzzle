/***
  this program is designed to solve fillin puzzles
  the main function is main/3, takes a puzzle file and a words file
  the third argument is the solution file
  @author Jiangbin Wang
  @id 728392
*/

% ensure the right version of transpose/2 is loaded
:- ensure_loaded(library(clpfd)). 

% the main fuction of this program
% take a puzzle file and a words file, solve then output a solution file
main(PuzzleFile, WordlistFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

% read the content of a file
read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

% read the content line by line
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

% read each line of the file and turn it into a list
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

% take the solved puzzle and write it to a given name file
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

% print row by row
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

% print characters
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

% if all the rows of a puzzle is the same length, then it's valid
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(samelength(Row), Rows).

% check if two arguments have same length
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

% take an puzzle list and a word list, return a solved version of puzzle
solve_puzzle(Puzzle0, WordList0, Puzzle) :-
    % transform '#' to solid and '_' to slot(_)
    map(transform, Puzzle0, Puzzle1), 
    % read all the slots of the puzzle both horizontally and vertically
    read_slots(Puzzle1, Slots1),
    transpose(Puzzle1, Puzzle2),
    read_slots(Puzzle2, Slots2),
    append(Slots1, Slots2, Slots),
    % filter out slots whose length is 1
    include(check_length, Slots, Slots0),
    % take the argument of each slot(_) to be unified with words
    map(transform1, Slots0, NewSlots),
    % fill all the slots with given words
    fill(NewSlots, WordList0),
    % transform the solution to suitable format
    map(transform2, Puzzle1, Puzzle).
    
% this is the map function in prolog
map(_, [], []).
map(P, [X|Xs], [Y|Ys]) :-
    call(P, X, Y),
    map(P, Xs, Ys).
    
% transform the format of puzzle
transform([], []).
transform(['#'|A], [solid|B]) :-
    transform(A, B).
transform(['_'|A], [slot(_)|B]) :-
    transform(A, B).
transform([X|A], [slot(X)|B]) :-
    X \= '#',
    X \= '_',
    transform(A, B).

% read the slots of puzzle
read_slots([], []).
read_slots([A|As], Slots) :-
    read_line_slots(A, B),
    read_slots(As, Bs),
    append(B, Bs, Slots).

% read each line of puzzle, ignoring '#' and change the rest into slots
read_line_slots([], []).    
read_line_slots([solid|A], B) :-
    read_line_slots(A, B).
read_line_slots(A, B) :-
    find_solid(A, N),
    length(Slot, N),
    append(Slot, Rest, A),
    read_line_slots(Rest, C),
    B = [Slot|C].
    
% find the index of next solid of end of line
find_solid([], 0).
find_solid([solid|_], 0).
find_solid([A|As], N) :-
    A \= solid,
    find_solid(As, N1),
    N is N1 + 1.
    
% check if the length of slot is larger than 1
check_length(X) :-
    length(X, N),
    N > 1.

% unify the slots and words
fill([], []).
fill(Slots, WordList) :-
    % find all the words that match each slot
    fill1(Slots, WordList, Choices),
    % choose slot with fewest matching words
    find_shortest(Choices, Choice, N),
    nth1(N, Slots, Slot, RestSlot),
    % choose one of the possible words
    member(X, Choice),
    Slot = X,
    % delete the word from word list
    select(X, WordList, RestWordList),
    % recursion
    fill(RestSlot, RestWordList).   
    
% generate all the possible words for each slot
fill1([], _, []).
fill1([A|As], WordList, [B|Bs]) :-
    setof(A, member(A, WordList), Words),
    B = Words,
    fill1(As, WordList, Bs).
    
% find the element of a list with shortest length as well as its index
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
    
% transform slot(X) to X
transform1([], []). 
transform1([A|As], [B|Bs]) :-
    arg(1, A, B),
    transform1(As, Bs).
    
% transform the format of solution
transform2([], []).
transform2([solid|A], ['#'|B]) :-
    transform2(A, B).
transform2([slot(X)|A], [X|B]) :-
    transform2(A, B).
    
    
    

    

    
    

    


























