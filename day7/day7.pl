:- use_module(library(charsio)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- initialization main.

%% parsing

list_of_numbers([N|Ns]) --> number(N), ",", list_of_numbers(Ns), !.
list_of_numbers([N])    --> number(N).



%% part 1

fuel([], _, 0).
fuel([P|Ps], Target, Fuel) :-
	fuel(Ps, Target, F1),
	D #= Target - P,
	F #= abs(D),
	Fuel #= F + F1.

fuel_candidates(Positions, Fuel) :-
	member(Target, Positions),
	fuel(Positions, Target, Fuel).

part1(Positions, Fuel) :-
	findall(F, fuel_candidates(Positions, F), Fs),
	min_list(Fs, Fuel).


%% part 2

sum_to_n(N, S) :- 
	N1 #= N + 1,
	S1 #= N * N1,
	S #= S1 // 2.

fuel2([], _, 0).
fuel2([P|Ps], Target, Fuel) :-
	fuel2(Ps, Target, F1),
	D #= Target - P,
	D1 #= abs(D),
	sum_to_n(D1, F),
	Fuel #= F + F1.

fuel_candidates2(Positions, Fuel) :-
	member(Target, Positions),
	fuel2(Positions, Target, Fuel).

part2(Positions, Fuel) :-
	findall(F, fuel_candidates2(Positions, F), Fs),
	min_list(Fs, Fuel).


%% tests

test_data(Positions) :-
	atom_codes("16,1,2,0,4,2,7,1,2,14", Cs),
	phrase(list_of_numbers(Positions), Cs).

test_part1 :-
	test_data(Positions),
	part1(Positions, Fuel),
	Fuel #= 37.


%% main

main :- 
	phrase_from_file(list_of_numbers(Positions), `input.txt`),
	part1(Positions, Fuel1),
	write('Part 1 : '), write(Fuel1), nl,
	part2(Positions, Fuel2),
	write('Part 2 : '), write(Fuel2), nl,
	halt(0).
