:- begin_tests(lib).
:- consult(lib).

cave(start, small).
cave(end, small).
cave(a, big).
cave(b, small).
cave(c, small).
cave(d, small).

edge(start, a).
edge(start, b).
edge(a, c).
edge(a, b).
edge(b, d).
edge(a, end).
edge(b, end).

test(find_route) :-
	findall(R, find_route(start, end, R), Rs),
	length(Rs, 10).

:- end_tests(lib).
