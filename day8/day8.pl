:- use_module(library(charsio)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- initialization main.


%% parsing

pattern(P) --> nonblanks(P).

patterns([P]) --> pattern(P).
patterns([P|Ps]) --> pattern(P), white, patterns(Ps).

entry(Patterns, Digits) -->
	patterns(Patterns), ` | `, patterns(Digits).

entry(E) --> entry(P, D), { 
		maplist(atom_codes, P1, P),
		maplist(atom_codes, D1, D),
		E = [P1, D1]
	}.

entries([]) --> [].
entries([E|Es]) --> entry(E), [10], entries(Es).


%% part 1

is_1478(Digits) :-
	atom_length(Digits, L),
	member(L, [2,3,4,7]).

find_1478s([], []).
find_1478s([D|Ds], Rs) :-
	is_1478(D), find_1478s(Ds, R1s), !, Rs = [D|R1s];
	find_1478s(Ds, Rs).

find_1478s_in_entry([_, Digits], Result) :-
	find_1478s(Digits, Result).

part1(Entries, N) :-
	maplist(find_1478s_in_entry, Entries, R),
	flatten(R, R1),
	length(R1, N).


%% part 2

constrain_0(P, [T, L1, R1, _, L2, R2, B]) :-
	length(P, 6),
	member(T, P),
	member(L1, P),
	member(R1, P),
	member(L2, P),
	member(R2, P),
	member(B, P).

constrain_1(P, [_, _, R1, _, _, R2, _]) :-
	length(P, 2),
	member(R1, P),
	member(R2, P).

constrain_2(P, [T, _, R1, M, L2, _, B]) :-
	length(P, 5),
	member(T, P),
	member(R1, P),
	member(M, P),
	member(L2, P),
	member(B, P).

constrain_3(P, [T, _, R1, M, _, R2, B]) :-
	length(P, 5),
	member(T, P),
	member(R1, P),
	member(M, P),
	member(R2, P),
	member(B, P).

constrain_4(P, [_, L1, R1, M, _, R2, _]) :-
	length(P, 4),
	member(L1, P),
	member(R1, P),
	member(M, P),
	member(R2, P).

constrain_5(P, [T, L1, _, M, _, R2, B]) :-
	length(P, 5),
	member(T, P),
	member(L1, P),
	member(M, P),
	member(R2, P),
	member(B, P).

constrain_6(P, [T, L1, _, M, L2, R2, B]) :-
	length(P, 6),
	member(T, P),
	member(L1, P),
	member(M, P),
	member(L2, P),
	member(R2, P),
	member(B, P).

constrain_7(P, [T, _, L1, _, _, L2, _]) :-
	length(P, 3),
	member(T, P),
	member(L1, P),
	member(L2, P).

constrain_9(P, [T, L1, L2, M, _, R2, B]) :-
	length(P, 6),
	member(T, P),
	member(L1, P),
	member(L2, P),
	member(M, P),
	member(R2, P),
	member(B, P).

constrain(P, W) :-
	length(P, 7);
	constrain_1(P, W); 
	constrain_7(P, W);
	constrain_4(P, W);
	constrain_2(P, W);
	constrain_3(P, W);
	constrain_5(P, W);
	constrain_0(P, W);
	constrain_6(P, W);
	constrain_9(P, W).

unconstrained(X) :-
	atom_codes(abcdefg, Cs),
	member(X, Cs).

constrain_wiring([], [T,L1,R1,M,L2,R2,B]) :-
	unconstrained(T),
	unconstrained(L1),
	unconstrained(R1),
	unconstrained(M),
	unconstrained(L2),
	unconstrained(R2),
	unconstrained(B),
	all_different([T,L1,R1,M,L2,R2,B]).

constrain_wiring([P|Ps], W) :-
	constrain_wiring(Ps, W),
	atom_codes(P, Cs),
	constrain(Cs, W).

determine_wiring(Ps, W) :-
	constrain_wiring(Ps, W), !.

match(W, D, N) :-
	length(D, 7), N is 8, !;
	constrain_1(D, W), N is 1, !;
	constrain_7(D, W), N is 7, !;
	constrain_4(D, W), N is 4, !;
	constrain_2(D, W), N is 2, !;
	constrain_3(D, W), N is 3, !;
	constrain_5(D, W), N is 5, !;
	constrain_0(D, W), N is 0, !;
	constrain_6(D, W), N is 6, !;
	constrain_9(D, W), N is 9.
	
eval_digit(W, D, N) :-
	atom_codes(D, Cs),
	match(W, Cs, N).

eval_digits(W, Ds, N) :-
	maplist(eval_digit(W), Ds, Ns),
	to_number(Ns, N).

to_number(Ds, N) :- to_number(Ds, N, _).
to_number([D], D, 1).
to_number([D|Ds], N, M) :-
	M #= M1 * 10,
	to_number(Ds, N1, M1),
	D1 #= D * M,
	N #= D1 + N1.

part2([Patterns, Digits], Ns) :-
	determine_wiring(Patterns, Mapping),
	eval_digits(Mapping, Digits, Ns).

%% tests

test_data(Entries) :-
	phrase_from_file(entries(Entries), `example.txt`).

part1_tests :-
	test_data(Entries),
	part1(Entries, 26).

part2_tests :-
	determine_wiring([acedgfb, cdfbe, gcdfa, fbcad, dab, cefabd, cdfgeb, eafb, cagedb, ab], W),
	atom_codes(deafgbc, W),
	eval_digits(W, [cdfeb,fcadb,cdfeb,cdbaf], 5353).

part2_tests2 :-
	test_data(Entries),
	maplist(part2, Entries, Ns),
	Ns = [8394, 9781, 1197, 9361, 4873, 8418, 4548, 1625, 8717, 4315].

%% main

main :-
	phrase_from_file(entries(Entries), `input.txt`),

	part1_tests,
	part1(Entries, R1),
	write('Part 1: '), write(R1), nl,

	part2_tests,
	part2_tests2,
	maplist(part2, Entries, Ns),
	sum_list(Ns, N),
	write('Part 2: '), write(N), nl,

	halt(0).
