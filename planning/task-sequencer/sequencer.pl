% vim: ft=prolog

:- module('sequencer', [ sequencer/2 ]).

:- use_module('task-sequencer/permutation', [ perm/2 ]).
:- use_module('task-sequencer/genetic', [ gera/ 2 ]).

:- use_module('env', [ task_sequence_alg/1 ]).

impls('permutations', permutation_sequencer:perm).
impls('genetic', genetic:gera).

sequencer(Tasks, Order) :-
    task_sequence_alg(Name),
    impls(Name, Alg),
    call(Alg, Tasks, Order).
