%  Author: James Stone 761353 ( + some supplied code !  for reading in and out the puzzles)

:- ensure_loaded(library(clpfd)). % get the right version of transpose.
% what should get run when compiled to an executible
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

%reads Filename to Content
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).
%reads a file Stream to Content
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
% Reads a line of text from Stream, also returns Last if it is the last line in file
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

% Prints a Puzzle to a SoultionFile
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% Prints a row to a file Stream
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% prints a character to a file Stream.
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% checks if a puzzle is valid
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

% checks if two lists are of the samelength
samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% solve_puzzle(Puzzle, Words, SolvedPuzzle)
% should hold when SolvedPuzzle is a solved version of Puzzle, with the
% empty slots filled in with words from WordList.  Puzzle and SolvedPuzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  Words is also a list of lists of
% characters, one list per word.


solve_puzzle(Puzzle, Words, SolvedPuzzle) :-
	get_cells(Puzzle, SolvedPuzzle),
	get_slots(SolvedPuzzle, Slots),
  solve(Slots, Words).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% get_cells(Rows, Result)
% converts Rows to a modified version of Rows where each cell has been parsed using get_cell
get_cells([], []). % overlysafe basecase
get_cells(Rows, Result) :-
    maplist(row_to_cells, Rows, Result).

% row_to_cells(Row, Result)
% converts a row to a modified version of Row where each cell has been parsed using get_cell
% a helper for get_cells
row_to_cells([], []).  % overlysafe basecase
row_to_cells(Row, Result) :-
    maplist(get_cell, Row, Result).

% get_cell
% parses a string of a cell to an Logical Variable if it isn't known.
% otherwise keeps it as a string
get_cell('_', _).
get_cell(Letter, Letter) :- Letter \= '_'.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve(Slots, Words)
% retruns true if slots is a solution for the words
solve([], []). % basecase
solve(Slots, Words) :-
	% try the "best" solutions first
  get_next_best_slot(Slots, Words, BestSlot),
  exclude(\=(BestSlot), Words, FitingWords),
  member(Word, FitingWords),
  BestSlot = Word,
  exclude(==(Word), Words, RemainingWords),
  exclude(==(BestSlot), Slots, RemainingSlots),
  solve(RemainingSlots, RemainingWords).

%  gets the slot with the fewset number of fitting words
get_next_best_slot([Slot|Slots], Words, BestSlot) :-
    get_words_fiting_slot(Slot, Words, Count),
    get_next_best_slot(Slots, Words, Count, Slot, BestSlot).

	% helper
	get_next_best_slot([], _, _, BestSlot, BestSlot). % basecase
	get_next_best_slot([Slot|Slots], Words, LowestFiting,
	  CurrentBestSlot, BestSlot) :-
	  get_words_fiting_slot(Slot, Words, Count),
	  (Count < LowestFiting ->
				% new better slot
	        NewCurrentBestSlot = Slot,
	        LowestFiting1 = Count;
				% continue
				NewCurrentBestSlot = CurrentBestSlot,
	      LowestFiting1 = LowestFiting
	  ),
	  get_next_best_slot(Slots, Words, LowestFiting1, NewCurrentBestSlot, BestSlot).

% gets list of words that fit in a slot (Words),
% Also returns number of words that fit (TotalFitting) rather than having to recalc using length or similar
get_words_fiting_slot(Slot, Words, TotalFitting) :-
  get_words_fiting_slot(Slot, Words, 0, TotalFitting).

	% helper
	get_words_fiting_slot(_, [], NumFitingAcc, NumFitingAcc).
	get_words_fiting_slot(Slot, [Word|Words], NumFitingAcc, Count) :-
	  (Slot \= Word ->
	      NewNumFitingAcc is NumFitingAcc;
				NewNumFitingAcc is NumFitingAcc + 1
	  ),
		get_words_fiting_slot(Slot, Words, NewNumFitingAcc, Count).
