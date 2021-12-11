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
	part1(Positions, Fuel),
	write('Part 1 : '), write(Fuel), nl,
	halt(0).
