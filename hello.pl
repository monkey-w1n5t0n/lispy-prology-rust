#!/usr/bin/env swipl

% Main hello world program
main :-
    write('\e[32mHello, World!\e[0m\n').

% Basic test framework
:- begin_tests(hello).

test(simple_addition) :-
    X is 2 + 2,
    assertion(X =:= 4),
    ansi_format([fg(green)], 'OK: 2 + 2 = 4~n', []).

test(failing_test) :-
    % This test is expected to fail
    X is 2 + 2,
    assertion(X =:= 5),
    ansi_format([fg(red)], 'Should not see this~n', []).

:- end_tests(hello).

% Entry point that runs both the hello world and tests
:- initialization(main, main).
:- initialization((run_tests, halt), main).