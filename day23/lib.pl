:- use_module(library(clpfd)).

pod(typeA, 1).
pod(typeA, 2).
pod(typeB, 1).
pod(typeB, 2).
pod(typeC, 1).
pod(typeC, 2).
pod(typeD, 1).
pod(typeD, 2).

multiplier(pod(typeA, _), 1).
multiplier(pod(typeB, _), 10).
multiplier(pod(typeC, _), 100).
multiplier(pod(typeD, _), 1000).

hallway(h1).
hallway(h2).
hallway(h3).
hallway(h4).
hallway(h5).
hallway(h6).
hallway(h7).
hallway(h8).
hallway(h9).
hallway(h10).
hallway(h11).

room(room1, 1).
room(room1, 2).
room(room2, 1).
room(room2, 2).
room(room3, 1).
room(room3, 2).
room(room4, 1).
room(room4, 2).

move_cost(room(X, 2), room(X, 1), 1).

move_cost(h1, h2, 1).
move_cost(h2, h4, 2).
move_cost(h4, h6, 2).
move_cost(h6, h8, 2).
move_cost(h8, h10, 2).
move_cost(h10, h11, 1).
move_cost(room(room1, 1), h2, 2).
move_cost(room(room1, 1), h4, 2).
move_cost(room(room2, 1), h4, 2).
move_cost(room(room2, 1), h6, 2).
move_cost(room(room3, 1), h6, 2).
move_cost(room(room3, 1), h8, 2).
move_cost(room(room4, 1), h8, 2).
move_cost(room(room4, 1), h10, 2).

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

move_pod(Pod, Positions, Target, NewPositions, Cost) :-
	select(position(Pod, From), Positions, UnchangedPositions),
	can_move(From, To, Positions, [From], C1),
	call(Target, To),
	NewPositions = [position(Pod, To)|UnchangedPositions],
	%% write(Pod), write(" "), write(From), write(" -> "), write(To), nl,
	multiplier(Pod, M),
	Cost #= C1 * M.

is_hallway(L) :- hallway(L).
is_room(L) :- L = room(_, _).
is_in_room(position(_, L)) :- is_room(L).

pods_organized(Positions) :-
	maplist(is_in_room, Positions),
	member(position(pod(typeA, 1), room(RoomA, _)), Positions),
	member(position(pod(typeA, 2), room(RoomA, _)), Positions),
	member(position(pod(typeB, 1), room(RoomB, _)), Positions),
	member(position(pod(typeB, 2), room(RoomB, _)), Positions),
	member(position(pod(typeC, 1), room(RoomC, _)), Positions),
	member(position(pod(typeC, 2), room(RoomC, _)), Positions),
	member(position(pod(typeD, 1), room(RoomD, _)), Positions),
	member(position(pod(typeD, 2), room(RoomD, _)), Positions).

move_pod_to_hallway(Positions, PodsMovedToHallway, NewPositions, Pod, Cost) :-
	member(position(Pod, From), Positions),
	From = room(_, _),
	not_member(Pod, PodsMovedToHallway),
	%% write("Move pod to hallway "), write(Pod), nl,
	move_pod(Pod, Positions, is_hallway, NewPositions, Cost).

move_pod_to_room(Positions, PodsMovedToRoom, NewPositions, Pod, Cost) :-
	member(position(Pod, From), Positions),
	not_member(Pod, PodsMovedToRoom),
	hallway(From),
	%% write("Move pod to room "), write(Pod), nl,
	move_pod(Pod, Positions, is_room, NewPositions, Cost).

organize_pods(Positions, TotalCost) :-
	organize_pods(Positions, [], [], TotalCost).

organize_pods(Positions, PodsMovedToHallway, PodsMovedToRoom, TotalCost) :-
	pods_organized(Positions), !;
	(
		move_pod_to_hallway(Positions, PodsMovedToHallway, NewPositions, Pod, Cost),
		organize_pods(NewPositions, [Pod|PodsMovedToHallway], PodsMovedToRoom, Cost1),
		TotalCost #= Cost + Cost1
	);
	(
		move_pod_to_room(Positions, PodsMovedToRoom, NewPositions, Pod, Cost),
		organize_pods(NewPositions, PodsMovedToHallway, [Pod|PodsMovedToRoom], Cost1),
		TotalCost #= Cost + Cost1
	).

:- begin_tests(lib).

init(Positions) :-
	Positions = [
		position(pod(typeA, 1), room(room1, 2)),
		position(pod(typeA, 2), room(room4, 2)),
		position(pod(typeB, 1), room(room1, 1)),
		position(pod(typeB, 2), room(room3, 1)),
		position(pod(typeC, 1), room(room2, 1)),
		position(pod(typeC, 2), room(room3, 2)),
		position(pod(typeD, 1), room(room2, 2)),
		position(pod(typeD, 2), room(room4, 1))
	].

test(pods_organized) :-
	Positions = [
		position(pod(typeA, 1), room(room1, 1)),
		position(pod(typeA, 2), room(room1, 2)),
		position(pod(typeB, 1), room(room2, 1)),
		position(pod(typeB, 2), room(room2, 2)),
		position(pod(typeC, 1), room(room3, 1)),
		position(pod(typeC, 2), room(room3, 2)),
		position(pod(typeD, 1), room(room4, 1)),
		position(pod(typeD, 2), room(room4, 2))
	],
	pods_organized(Positions).

test(initial_moves) :-
	init(Positions),
	findall(Cost, move_pod(pod(typeB, 1), Positions, _, Cost), Costs),
	Costs = [20,20,40,30,60,60,80|_].

:- end_tests(lib).