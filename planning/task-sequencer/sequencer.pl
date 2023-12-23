% vim: ft=prolog

:- module('sequencer', [ sequencer/3 ]).

:- use_module('task-sequencer/permutation', [ perm/3 ]).
:- use_module('task-sequencer/genetic', [ gera_prin/ 3 ]).

:- use_module('loader/http-loader', [ getrobotpos/3 ]).

:- use_module('env', [ task_sequence_alg/1 ]).

impls('permutations', permutation_sequencer:perm).
impls('genetic', genetic:gera_prin).

sequencer(Tasks, Order, Robot) :-
    [(start(B, F, _, _), _, _)|_] = Tasks,
    getrobotpos(B, F, Robot),

    % task_sequence_alg(Name),
    % format('Status: antes~n~n'),
    perm(Robot, Tasks, Order).
    % gera_prin(Robot, Tasks, Order).
    % format('Status: depois~n~n').
    % impls(Name, Alg),
    % call(Alg, Tasks, Order).
