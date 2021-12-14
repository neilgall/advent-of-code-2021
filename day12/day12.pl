:- initialization main.
:- consult(lib).
:- consult(input).

part1(N) :-
	findall(R, find_route(start, end, R), Rs),
	length(Rs, N).

main :-
	part1(R1),
	write("Part 1: "), write(R1), nl,
	halt(0).
