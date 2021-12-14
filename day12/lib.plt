:- begin_tests(lib).
:- consult(lib).
:- consult(testdata).

test(find_route) :-
	find_number_of_routes(start, end, 10).

test(find_route_visiting_one_small_cave_twice) :-
	find_number_of_routes_visiting_one_small_cave_twice(start, end, 36).

:- end_tests(lib).
