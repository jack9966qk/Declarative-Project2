/*  COMP30019 Project 2 -- Fillin Puzzle Solver
        QIAN Kuan (Jack) [686464]
        October 2016

This file contains all predicates for the fillin puzzle solver, for
project 2 in COMP30019, Declarative Programming, in the University of
Melbourne.

A fillin puzzle (sometimes called a fill-it-in) is like a crossword
puzzle, except that instead of being given obscure clues telling which
words go where, you are given a list of all the words to place in the
puzzle, but not told where they go. Wikipedia page for more
information: https://en.wikipedia.org/wiki/Fill-In_(puzzle)

For this project, a fillin puzzle is represented as a plain text file
for the squares and another plain text file for the words to be filled
into. The puzzle solver reads the two files, and generates the
solution to another file.

The puzzle file contains a number of lines each with the same number
of characters, to form a rectangle. The characters in this file should
all be either an underline character (_) indicating a fill-able
square, a hash character (#) indicating a solid, non-fill-able square,
or a letter or digit, indicating a pre-filled square. The output
SolutionFile have the same format (except that it should be filled, so
it should not contain underlines). The word list file is simply a text
file with one word per line.

Example:

    Puzzle file content:
        ____
        ___#
        ____

    Word list file content:
        SOS
        SOT
        ALEC
        BOER
        BRIE
        (and more lines...)

    Solution file content:
        boat
        art#
        need

To use the solver, simply call the `main` predicate with related file
names, see the predicate description for more details.

*/

:- use_module(library(clpfd), []).

% main(PuzzleFile, WordlistFile, SolutionFile)
% the main predicate for solving fillin puzzles. PuzzleFile,
% WordlistFile, and SolutionFile are all filenames to the related
% files. Predicate should hold when the puzzle is successfully read
% and solved, with the solution written into SolutionFile
main(PuzzleFile, WordlistFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

% read_file(Filename, Content)
% should hold when Filename is valid, and Content is the content of
% file, as a list of lists of characters, each list of characters
% representing a line in the file.
read_file(Filename, Content) :- open(Filename, read, Stream),
read_lines(Stream, Content), close(Stream).

% read_lines(Stream, Content)
% should hold when Stream is valid file stream, and Content is the
% content of the stream, as a list of lists of characters, each list
% of characters representing a line in the file.
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

% read_line(Stream, Line, Last)
% should hold when Stream is a vaild file stream, Line is a list of
% characters representing the next line of the stream, and Last being
% a boolean indicating whether the end_of_file has been reached.
read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ;       Char = '\n'
    ->  Line = [],
        Last = false
    ;   Line = [Char|Line1],
        read_line(Stream, Line1, Last)
    ).

% print_puzzle(SolutionFile, Puzzle)
% should hold when SolutionFile is the filename of the solution file,
% puzzle is a 2 dimensional list for the fillin puzzle, and its
% content written to the solution file.
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

% print_row(Stream, Row)
% should hold when Stream is a valid file stream, Row is a list of
% characters, and Row is written into Stream as one line.
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

% put_puzzle_char(Stream, Char)
% should hold when Stream is a valid file stream, Char is a character,
% and the character is written into Stream.
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

% valid_puzzle(Puzzle)
% should hold when Puzzle is valid in its format (2 dimensional list),
% with all rows in the same length.
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(samelength(Row), Rows).

% samelength(L1, L2)
% should hold when L1 and L2 are two lists with the same length.
samelength([], []).
samelength([_|L1], [_|L2]) :-
    same_length(L1, L2).

% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
solve_puzzle(Puzzle0, WordList, Filled) :-
    slots(Puzzle0, Slots, Filled),
    solve(Slots, WordList).

% slots(Puzzle, Slots, Filled)
% should hold when Puzzle is a valid puzzle, Slots is a list of all
% slots in the puzzle, with each slot represented as a list of
% variables that should match one word in the word list, and Filled is
% the puzzle filled by the variables in Slots in place.
slots(Puzzle, Slots, Filled) :-
    horizontal_slots(Puzzle, SH, Filled),
    vertical_slots(Puzzle, SV, Filled),
    append(SH, SV, Slots).

% vertical_slots(Puzzle, Slots, Filled)
% should hold when Puzzle is a valid puzzle, Slots is a list of all
% vertical slots in the puzzle, and Filled is the puzzle filled by the
% variables in Slots in place.
vertical_slots(Puzzle, Slots, Filled) :-
    clpfd:transpose(Puzzle, TPuzzle),
    horizontal_slots(TPuzzle, Slots, TFilled),
    clpfd:transpose(Filled, TFilled).

% horizontal_slots(Puzzle, Slots, Filled)
% should hold when Puzzle is a valid puzzle, Slots is a list of all
% horizontal slots in the puzzle, and Filled is the puzzle filled by
% the variables in Slots in place.
horizontal_slots([], [], []).
horizontal_slots([Line|Lines], Slots, Filled) :-
    line_slots(Line, S, LineFilled),
    horizontal_slots(Lines, MoreS, MoreFilled),
    append(S, MoreS, Slots),
    Filled = [LineFilled | MoreFilled].

% line_slots(Line, Slots, Filled)
% should hold when Line is a line (list of characters) in a valid
% puzzle, Slots is a list of all slots in the line, and Filled is the
% puzzle filled by the variables in Slots in place.
line_slots(Line, Slots, Filled) :-
    line_first_slot(Line, Slot, Following, SomeFilled),
    (       Slot = []
    ->  Filled = SomeFilled,
            Slots = []
    ;       Slot = [_]
    ->  line_slots(Following, Slots, MoreFilled),
            append(SomeFilled, MoreFilled, Filled)
    ;       line_slots(Following, MoreSlots, MoreFilled),
            Slots = [Slot | MoreSlots],
            append(SomeFilled, MoreFilled, Filled)
    ).

% line_first_slot(Line, Slots, Following, Filled)
% should hold when Line is a line (list of characters) in a valid
% puzzle, Slot is the first slot in the line, Following is the
% remaining characters in the line as a list and Filled is the puzzle
% filled by the variables in Slots in place.
line_first_slot(Line, Slot, Following, Filled) :-
    skip_solid(Line, Striped, Solid),
    get_slot(Striped, Slot, Following, MoreFilled),
    append(Solid, MoreFilled, Filled).

% get_slot(Line, Slots, Following, Filled)
% should hold when Line is a line (list of characters) in a valid
% puzzle, Slot is is the first slot of the line, starting from the
% first character, Following is the remaining characters in the line
% as a list and Filled is the puzzle filled by the variables in Slots
% in place.
get_slot([], [], [], []).
get_slot([C|Cs], Slot, Following, Filled) :-
    (   C = '#'
    ->  Slot = [],
            Following = [C|Cs],
            Filled = []
    ;       C = '_'
    ->  get_slot(Cs, Rest, Following, MoreFilled),
            Slot = [X|Rest],
            Filled = [X|MoreFilled]
    ;       get_slot(Cs, Rest, Following, MoreFilled),
            Slot = [C|Rest],
            Filled = [C|MoreFilled]
    ).

% skip_solid(Line, Following, SolidPart)
% should hold when Line is a line in the puzzle, SolidPart is the
% solid part of the line starting from the first character, and
% Following contains characters after the solid part.
skip_solid([], [], []).
skip_solid([H|T], Following, SolidPart) :-
    (       H = '#'
    ->  skip_solid(T, Following, MoreSolid),
            SolidPart = ['#' | MoreSolid]
    ;       Following = [H|T],
            SolidPart = []
    ).

% solve(Slots, WordList)
% should hold when Slots contains the slots in a puzzle, WordList
% contains the words in the puzzle, and the puzzle is solved, that is,
% all variables in Slots are unified with characters in WordList, with
% each word in word list matched only once.
solve([], []).
solve(Slots, WordList) :-
    pick_slot(Slots, WordList, One),
    select(One, Slots, More),
    select(One, WordList, NWordList),
    solve(More, NWordList).

% pick_slot(Slots, WordList, Best)
% should hold when Slots contains the slots in a puzzle, WordList
% contains the words in the puzzle, and Best is the most preferred
% slot in Slots to fill (that is, to unify with a word in WordList).
pick_slot(Slots, WordList, Best) :-
    minimum_by(num_unifiable(WordList), Slots, Best).

% num_unifiable(WordList, Slot, Num)
% should hold when Slot is a slot in a puzzle, WordList is the word
% list in the puzzle, and Num is the number of words in word list that
% can be used to fill the Slot, that is, to be unified with Slot.
num_unifiable([], _, 0).
num_unifiable([H|T], Slot, Num) :-
    (       Slot \= H
    ->  num_unifiable(T, Slot, Num)
    ;       num_unifiable(T, Slot, N),
            Num is N+1
    ).

% minimum_by(P, List, MinItem)
% should hold when P is a predicate in format of P(Item, Value), where
% Item is an Item in List, and Value is the desired value to compare
% with, List is a list of items, and MinItem is the item with minimum
% value by the predicate.
minimum_by(P, List, MinItem) :-
    [H|_] = List,
    call(P, H, Value),
    minimum_by_acc(P, List, MinItem, H, Value).

% minimum_by_acc(P, List, MinItem, PrevMinItem, PrevMinValue)
% should hold when P is a predicate in format of P(Item, Value), where
% Item is an Item in List, and Value is the desired value to compare
% with, List is a list of items, and MinItem is the item with minimum
% value by the predicate so far, compared with PrevMinItem being the
% previous minimum item and PrevMinValue being its value.
minimum_by_acc(_, [], PrevMinItem, PrevMinItem, _).
minimum_by_acc(P, [H|T], MinItem, PrevMinItem, PrevMinValue) :-
    call(P, H, Value),
    (       Value < PrevMinValue
    ->  minimum_by_acc(P, T, MinItem, H, Value)
    ;       minimum_by_acc(P, T, MinItem, PrevMinItem, PrevMinValue)
    ).
