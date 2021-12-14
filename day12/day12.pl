:- consult(lib).
:- consult(input).

part1(N) :-
	find_number_of_routes(start, end, N).

part2(N) :-
	find_number_of_routes_visiting_one_small_cave_twice(start, end, N).

main :-
	part1(R1),
	write("Part 1: "), write(R1), nl,
	part2(R2),
	write("Part 2: "), write(R2), nl,
	halt(0).
