% vim: ft=prolog

:- module('sequencer', [ sequencer/3 ]).

:- use_module('task-sequencer/permutation', [ perm/2 ]).
:- use_module('task-sequencer/genetic', [ gera_prin/ 3 ]).

:- use_module('env', [ task_sequence_alg/1 ]).

impls('permutations', permutation_sequencer:perm).
impls('genetic', genetic:gera_prin).

sequencer(Robot,Tasks, Order) :-
    % task_sequence_alg(Name),
    % format('Status: antes~n~n'),
    gera_prin(Robot, Tasks, Order).
    % format('Status: depois~n~n').
    % impls(Name, Alg),
    % call(Alg, Tasks, Order).
    
