% You can use this code to get started with your fillin puzzle solver.

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

solve_puzzle(Puzzle, Words, SolvedPuzzle) :-
	get_cells(Puzzle, CelledPuzzle),
	get_slots(CelledPuzzle, Slots),
	print(Words),
	print(Slots),
	SolvedPuzzle = Puzzle.


get_cells([], []).
get_cells(Rows, Result) :-
    maplist(row_to_cells, Rows, Result).


row_to_cells([], []).
row_to_cells(Row, Result) :-
    maplist(get_cell, Row, Result).


get_cell('_', _).
get_cell(Letter, Letter) :- Letter \= '_'.





get_slots(Puzzle, Slots) :-
    get_row_slots(Puzzle, RowSlots),
    include(not_single_letter_word, RowSlots, FilteredRowSlots), % filter single letter word slots
    transpose(Puzzle, TransposedPuzzle),
    get_row_slots(TransposedPuzzle, ColumnSlots),
    include(not_single_letter_word, ColumnSlots, FilteredColumnSlots), % filter single letter word slots
    append(FilteredRowSlots, FilteredColumnSlots, Slots).

not_single_letter_word(Word) :-
    length(Word, NumLetters),
    NumLetters > 1.

get_row_slots([], []).
get_row_slots([Row|Rows], Slots) :-
    get_slots_in_row(Row, RowSlots),
    append(RowSlots, RemainderSlots, Slots),
    get_row_slots(Rows, RemainderSlots).

get_slots_in_row(Row, Slots) :-
    get_slots_in_row(Row, [], Slots).

get_slots_in_row([], [], []).
get_slots_in_row([], CurrentSlot, [CurrentSlot]) :-
    CurrentSlot \= [].
get_slots_in_row([Cell|Cells], CurrentSlot, Slots) :-
    Cell == '#',
    Slots = [CurrentSlot|RemainderSlots],
    get_slots_in_row(Cells, [], RemainderSlots).
get_slots_in_row([Cell|Cells], CurrentSlot, Slots) :-
    Cell \== '#',
    append(CurrentSlot, [Cell], RemainderSlots),
    get_slots_in_row(Cells, RemainderSlots, Slots).
