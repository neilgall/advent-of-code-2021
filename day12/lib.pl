
not_member(X, Ys) :-
	maplist(dif(X), Ys), !.

visit_all_caves(Caves) :-
	findall(Cave, cave(Cave, _), Caves).

only_revisit_big_caves(Cave, Caves) :-
	cave(Cave, big), !;
	not_member(Cave, Caves).

can_move(From, To) :-
	edge(From, To); edge(To, From).

route(Start, End, [End], _) :- 
	can_move(Start, End).

route(Start, End, [Cave|Route], Visited) :-
	can_move(Start, Cave),
	only_revisit_big_caves(Cave, Visited),
	route(Cave, End, Route, [Cave|Visited]).

find_route(Start, End, [Start|Route]) :-
	route(Start, End, Route, [Start, End]).
