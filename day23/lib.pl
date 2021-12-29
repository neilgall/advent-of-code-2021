:- use_module(library(clpfd)).

multiplier(podA1, 1).
multiplier(podA2, 2).
multiplier(podB1, 10).
multiplier(podB2, 10).
multiplier(podC1, 100).
multiplier(podC2, 100).
multiplier(podD1, 1000).
multiplier(podD2, 1000).

location(h1).
location(h2).
location(h3).
location(h4).
location(h5).
location(h6).
location(h7).
location(h8).
location(h9).
location(h10).
location(h11).
location(r1a).
location(r1b).
location(r2a).
location(r2b).
location(r3a).
location(r3b).
location(r4a).
location(r4b).

move_cost(h1,h2,1).
move_cost(h2,h4,2).
move_cost(h4,h6,2).
move_cost(h6,h8,2).
move_cost(h8,h10,2).
move_cost(h10,h11,1).
move_cost(r1a,h2,2).
move_cost(r1a,h4,2).
move_cost(r1b,r1a,1).
move_cost(r2a,h4,2).
move_cost(r2a,h6,2).
move_cost(r2b,r2a,1).
move_cost(r3a,h6,2).
move_cost(r3a,h8,2).
move_cost(r3b,r3a,1).
move_cost(r4a,h10,2).
move_cost(r4a,h10,2).
move_cost(r4b,r4a,1).

not_member(X, Ys) :-
	maplist(dif(X), Ys), !.

not_occupied(X, Occupied) :-
    \+ memberchk(position(_, X), Occupied).

can_move_direct(X, Y, Occupied, Cost) :-
	(move_cost(X, Y, Cost); move_cost(Y, X, Cost)),
	not_occupied(Y, Occupied).

can_move(X, Y, Occupied, _, Cost) :-
	can_move_direct(X, Y, Occupied, Cost).

can_move(X, Y, Occupied, Visited, Cost) :-
	can_move_direct(X, Z, Occupied, C1),
	not_member(Z, Visited),
	can_move(Z, Y, Occupied, [Z|Visited], C2),
	Cost #= C1 + C2.

move_pod(Pod, Positions, NewPositions, Cost) :-
	select(position(Pod, From), Positions, UnchangedPositions),
	can_move(From, To, Positions, [From], C1),
	NewPositions = [position(Pod, To)|UnchangedPositions],
	multiplier(Pod, M),
	Cost #= C1 * M.

:- begin_tests(lib).

init(Positions) :-
	Positions = [
		position(podA1, r1b),
		position(podA2, r4b),
		position(podB1, r1a),
		position(podB2, r3a),
		position(podC1, r2a),
		position(podC2, r3b),
		position(podD1, r2b),
		position(podD2, r4a)
	].

test(initial_moves) :-
	init(Positions).

:- end_tests(lib).