:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

%% parsing

digit(D) --> [D], { char_type(D, decimal_digit) }.

digits([D|Ds]) --> digit(D), digits(Ds), !.
digits([D])    --> digit(D).

number(N) --> digits(Ds), { number_chars(N, Ds) }.

draw([N|Ns]) --> number(N), ",", draw(Ns), !.
draw([N])    --> number(N).

board([R|Rs]) --> row(R), "\n", board(Rs), !.
board([R])    --> row(R).

row([N|Ns]) --> whitespace, number(N), row(Ns), !.
row([N])    --> whitespace, number(N).

whitespace --> "  ", !; " ", !; "", !.

boards([B|Bs]) --> board(B), "\n\n", boards(Bs), !.
boards([B])    --> board(B).

input(Draw, Boards) --> draw(Draw), "\n\n", boards(Boards), "\n".

load(File, Draw, Boards) :-
	phrase_from_file(input(Draw, Boards), File).


%% model

match_row(_, [], []).
match_row(N, [N|Xs], [x|Xs]) :- !.
match_row(N, [X|Xs], [X|Ys]) :- match_row(N, Xs, Ys).

match_board(_, [], []).
match_board(N, [X|Xs], [Y|Ys]) :-
	match_row(N, X, Y), match_board(N, Xs, Ys), !.

match_boards(_, [], []).
match_boards(N, [X|Xs], [Y|Ys]) :-
	match_board(N, X, Y), match_boards(N, Xs, Ys), !.

winning_row([x]).
winning_row([x|Xs]) :- winning_row(Xs), !.

winning_board_([R])    :- winning_row(R).
winning_board_([R|Rs]) :- winning_row(R); winning_board_(Rs), !.

winning_board(B, true) :-
	winning_board_(B);
	transpose(B, B2), winning_board_(B2).

winning_board(_, false).

score_row([], 0).
score_row([x|Xs], S) :- score_row(Xs, S), !.
score_row([X|Xs], S) :- score_row(Xs, S2), S is X + S2.

score_board([R], S) :- score_row(R, S), !.
score_board([R|Rs], S) :- 
	score_row(R, S1),
	score_board(Rs, S2),
	S is S1 + S2.


%% game

win(Draw, Boards, Score) :-
	tfilter(winning_board, Boards, [Win]),
	score_board(Win, S),
	Score is Draw * S.

game([D|Ds], Boards, Score) :-
	match_boards(D, Boards, NextBoards),
	(win(D, NextBoards, Score), !; game(Ds, NextBoards, Score)).

game([], _, 0).



%% tests

test_game :-
	load('test.txt', Draw, Boards),
	game(Draw, Boards, Score),
	Score is 4512.


%% problems

part1(Draw, Boards, Score) :-
	game(Draw, Boards, Score).

main :-
	test_game,
	load('input.txt', Draw, Boards),
	part1(Draw, Boards, Part1),
	write('Part 1 : '), write(Part1), nl.

