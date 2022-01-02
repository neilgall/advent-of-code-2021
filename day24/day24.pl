:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

% parsing

register_name(w) --> "w".
register_name(x) --> "x".
register_name(y) --> "y".
register_name(z) --> "z".

unary_instruction(unary(I, R)) --> 
    nonblanks(N),
    { atom_codes(I, N), member(I, [inp]) },
    whites,
    register_name(R).

binary_instruction(binary(I, R, Arg2)) --> 
    nonblanks(N),
    { atom_codes(I, N), member(I, [add, sub, mul, div, mod, eql]) },
    whites,
    register_name(R),
    whites,
    (
        (register_name(R2), { Arg2 = reg(R2) }, !);
        (number(V), { Arg2 = const(V) }, !)
    ).

instruction(I) -->
    unary_instruction(I), !;
    binary_instruction(I).

instructions(Is) --> 
    sequence(instruction, "\n", Is).


% virtual machine

new_vm(Instructions, Data, VM) :-
    list_to_assoc([w-0, x-0, y-0, z-0], Regs),
    VM = vm(
        Instructions,
        context(Data, Regs)
    ).

exec_unary(context(Data, Regs), context(Data2, Regs2), Op, R) :-
    get_assoc(R, Regs, V),
    call(Op, Data, Data2, V, V2),
    put_assoc(R, Regs, V2, Regs2).

eval_arg2(_, const(V), V).
eval_arg2(Regs, reg(R), V) :- get_assoc(R, Regs, V).

exec_binary(context(D, Regs), context(D, Regs2), Op, R, Arg2) :-
    get_assoc(R, Regs, V1),
    eval_arg2(Regs, Arg2, V2),
    call(Op, V1, V2, V3),
    put_assoc(R, Regs, V3, Regs2).

inp([D|Ds], Ds, _, D).

add(A, B, C) :- C #= A + B.
sub(A, B, C) :- C #= A - B.
mul(A, B, C) :- C #= A * B.
div(A, B, C) :- C #= div(A, B).
mod(A, B, C) :- C #= mod(A, B).
eql(A, B, C) :- (A #= B, C #= 1); (A #\= B, C #= 0).

step(vm([unary(Op, R)|Is], InContext), vm(Is, OutContext)) :-
    exec_unary(InContext, OutContext, Op, R).
    % write("Unary "), write(Op), write(" -> "), write(OutContext), nl.

step(vm([binary(Op, R, Arg2)|Is], InContext), vm(Is, OutContext)) :-
    exec_binary(InContext, OutContext, Op, R, Arg2).
    % write("Binary "), write(Op), write(" -> "), write(OutContext), nl

run(vm([], context(_, Regs)), Regs) :- !.

run(VM, Regs) :-
    step(VM, VM2),
    run(VM2, Regs).


% problems

part1(Instructions, ModelNumber) :-
    [D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14] ins 1..9,
    Data = [D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14],
    new_vm(Instructions, Data, VM),
    run(VM, Output),
    get_assoc(z, Output, 1),
    number_digits(ModelNumber, _, Data),
    write(ModelNumber), nl,
    labeling([max(ModelNumber)], Data).

load(Instructions) :-
    phrase_from_file(instructions(Instructions), 'input.txt').

main :-
    load(Instructions),
    part1(Instructions, ModelNumber),
    write("Part 1: "), write(ModelNumber), nl.

:- begin_tests(day24).

test(parse_inp_w) :-
    phrase(instruction(I), `inp w`),
    I = unary(inp, w).

test(parse_inp_z) :-
    phrase(instruction(I), `inp z`),
    I = unary(inp, z).

test(parse_add_w_x) :-
    phrase(instruction(I), `add w x`),
    I = binary(add, w, reg(x)).

test(parse_sub_y_z) :-
    phrase(instruction(I), `sub y z`),
    I = binary(sub, y, reg(z)).

test(parse_add_w_42) :-
    phrase(instruction(I), `add w 42`),
    I = binary(add, w, const(42)).

test(parse_eql_z_minus6) :-
    phrase(instruction(I), `eql z -6`),
    I = binary(eql, z, const(-6)).

test(parse_instuctions) :-
    phrase(instructions(Is), `inp w\nadd w z\nmul z 42\nsub x y`),
    Is = [
        unary(inp, w),
        binary(add, w, reg(z)),
        binary(mul, z, const(42)),
        binary(sub, x, reg(y))
    ].

test(vm_inp) :-
    new_vm([unary(inp, w)], [42], VM),
    step(VM, VM2),
    VM2 = vm([], context([], Regs)),
    get_assoc(w, Regs, 42).

test(vm_add) :- 
    new_vm([unary(inp, x), unary(inp, y), binary(add, x, reg(y))], [20, 22], VM),
    step(VM, VM2),
    step(VM2, VM3),
    step(VM3, VM4),
    VM4 = vm([], context([], Regs)),
    get_assoc(x, Regs, 42).

test(vm_run) :-
    new_vm([unary(inp, x), unary(inp, y), binary(add, x, reg(y))], [20, 22], VM),
    run(VM, Regs),
    get_assoc(x, Regs, 42).

:- end_tests(day24).