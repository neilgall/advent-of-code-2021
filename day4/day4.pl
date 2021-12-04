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

win_score(Draw, Win, Score) :-
	score_board(Win, S),
	Score is Draw * S.

wins(Draw, Boards, BoardsLeft, Scores) :-
	tfilter(winning_board, Boards, Wins),
	maplist(win_score(Draw), Wins, Scores),
	foldl(select, Wins, Boards, BoardsLeft).

game([D|Ds], Boards, Scores) :-
	match_boards(D, Boards, NextBoards),
	wins(D, NextBoards, BoardsLeft, WinScores),
	game(Ds, BoardsLeft, MoreScores),
	append(WinScores, MoreScores, Scores).

game([], _, []).


%% problems

part1(Draw, Boards, Score) :-
	game(Draw, Boards, [Score|_]), !.

part2(Draw, Boards, LastScore) :-
	game(Draw, Boards, Scores),
	reverse(Scores, [LastScore|_]), !.


%% tests

test_part1 :-
	load('test.txt', Draw, Boards),
	part1(Draw, Boards, Score),
	Score is 4512.

test_part2 :-
	load('test.txt', Draw, Boards),
	part2(Draw, Boards, Score),
	Score is 1924.


main :-
	test_part1,
	test_part2,
	load('input.txt', Draw, Boards),
	part1(Draw, Boards, Part1),
	write('Part 1 : '), write(Part1), nl,
	part2(Draw, Boards, Part2),
	write('Part 2 : '), write(Part2), nl.

