    :- use_module(library(clpfd)).

pos_in_target(pos(X,Y), target(pos(X1,Y1), pos(X2,Y2))) :-
    X1 #=< X, X #=< X2,
    Y1 #=< Y, Y #=< Y2.

pos_beyond_target(pos(X,Y), target(pos(_,Y1), pos(X2,_))) :-
    X #> X2, !;
    Y #< Y1.

pos_step(pos(X1,Y1), velocity(VX,VY), pos(X2,Y2)) :-
    X2 #= X1 + VX,
    Y2 #= Y1 + VY.

velocity_step(velocity(X1,Y1), velocity(X2,Y2)) :-
    Y2 #= Y1 - 1,
    (
        X1 #> 0, X2 #= X1 - 1, !;
        X1 #< 0, X2 #= X1 + 1, !;
        X1 #= 0, X2 #= 0
    ).

probe_step(probe(Pos1, Velocity1), probe(Pos2, Velocity2)) :-
    pos_step(Pos1, Velocity1, Pos2),
    velocity_step(Velocity1, Velocity2).


probe_hits_target(probe(Pos, _), Target, MaxY) :-
    pos_in_target(Pos, Target),
    Pos = pos(_, MaxY),
    !.

probe_hits_target(probe(Pos, Velocity), Target, MaxY) :-
    \+ pos_beyond_target(Pos, Target),
    probe_step(probe(Pos, Velocity), Next),
    probe_hits_target(Next, Target, MaxY1),
    Pos = pos(_, Y),
    MaxY #= max(Y, MaxY1).

velocities_hitting_target(Target, MaxY, velocity(VX, VY)) :-
    between(0, 20, VX),
    between(1, 200, VY),
    probe_hits_target(probe(pos(0,0),velocity(VX,VY)), Target, MaxY).

find_velocity_for_max_height(Target, MaxY, Velocity) :-
    findall(result(MaxY, Velocity),
            velocities_hitting_target(Target, MaxY, Velocity),
            Solutions),
    sort(1, @>, Solutions, Results),
    Results = [result(MaxY, Velocity)|_].


example_target(Target) :-
    Target = target(pos(20,-10),pos(30,-5)).

day17_target(Target) :-
    Target = target(pos(138,-125),pos(184,-71)).


main :- 
    day17_target(T),
    find_velocity_for_max_height(T, MaxY, _),
    write("Part 1 "), write(MaxY), nl,
    halt(0).


:- begin_tests(day17).

test(step_arithmetic) :- 
    probe_step(
        probe(pos(0,0), velocity(6,3)),
        probe(pos(6,3), velocity(5,2))
    ),
    probe_step(
        probe(pos(0,0), velocity(6,-3)),
        probe(pos(6,-3), velocity(5,-4))
    ),
    probe_step(
        probe(pos(0,0), velocity(0, 2)),
        probe(pos(0,2), velocity(0, 1))
    ).

test(in_target_arithmetic) :-
    T = target(pos(10,10),pos(15,15)),
    pos_in_target(pos(15,12), T),
    pos_in_target(pos(12,15), T),
    \+ pos_in_target(pos(9,12), T),
    \+ pos_in_target(pos(16,12), T),
    \+ pos_in_target(pos(12,9), T),
    \+ pos_in_target(pos(12,16), T).

test(beyond_target_arithmetic) :-
    example_target(T),
    pos_beyond_target(pos(31,-7), T),
    pos_beyond_target(pos(25,-11), T),
    \+ pos_beyond_target(pos(25,-4), T),
    \+ pos_beyond_target(pos(15,-7), T).    

test(probe_hits_target) :-
    example_target(T),
    probe_hits_target(probe(pos(0,0),velocity(7,2)), T, _),
    probe_hits_target(probe(pos(0,0),velocity(6,3)), T, _),
    probe_hits_target(probe(pos(0,0),velocity(9,0)), T, _).

test(probe_misses_target) :-
    example_target(T),
    \+ probe_hits_target(probe(pos(0,0),velocity(17,-4)), T, _).

test(maximum_y) :-
    example_target(T),
    probe_hits_target(probe(pos(0,0),velocity(6,9)), T, 45).    

test(find_velocity_for_max_height) :-
    example_target(T),
    find_velocity_for_max_height(T, MaxY, Velocity),
    MaxY = 45,
    Velocity = velocity(6, 9).

:- end_tests(day17).
