not_member(X, Ys) :-
    maplist(dif(X), Ys), !.

only_revisit_big_caves(Cave, Caves) :-
	cave(Cave, big), !;
	not_member(Cave, Caves).

can_move(From, To) :-
	edge(From, To); edge(To, From).

route(Start, End, [End], _, _) :- 
	can_move(Start, End).

route(Start, End, [Cave|Route], Visited, AllowTwice) :-
	can_move(Start, Cave),
	(
		only_revisit_big_caves(Cave, Visited),
		route(Cave, End, Route, [Cave|Visited], AllowTwice);

		cave(Cave, small),
		memberchk(Cave, Visited),
		memberchk(Cave, AllowTwice),
		route(Cave, End, Route, Visited, [])
	).

find_route(Start, End, [Start|Route]) :-
	route(Start, End, Route, [Start, End], []).

find_route_visiting_one_small_cave_twice(Start, End, [Start|Route]) :-
	cave(AllowTwice, small),
	AllowTwice \= Start,
	AllowTwice \= End,
	route(Start, End, Route, [Start, End], [AllowTwice]).


find_number_of_routes(Start, End, N) :-
	findall(R, find_route(Start, End, R), Rs),
	length(Rs, N).

find_number_of_routes_visiting_one_small_cave_twice(Start, End, N) :-
	findall(R, find_route_visiting_one_small_cave_twice(Start, End, R), Rs),
	% could not find a way to avoid some duplicate routes
	sort(Rs, Rs1),
	length(Rs1, N).