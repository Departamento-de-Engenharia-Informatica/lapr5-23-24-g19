% vim: ft=prolog

:- module('sequencer', [
    sequencer/4,
    sequence_algs/1
]).

:- use_module('task-sequencer/permutation', [ perm/3 ]).
:- use_module('task-sequencer/genetic', [ gera/ 3 ]).

:- use_module('loader/http-loader', [ getrobotpos/3 ]).

:- use_module('env', [ task_sequence_alg/1 ]).

impls('permutations', permutation_sequencer:perm).
impls('genetic', genetic:gera).

sequencer(AlgName, Tasks, Order, Robot) :-
    [(start(B, F, _, _), _, _)|_] = Tasks,
    getrobotpos(B, F, Robot),

    % format('Status: antes~n~n'),
    % perm(Robot, Tasks, Order).
    % gera_prin(Robot, Tasks, Order).
    % format('Status: depois~n~n').

    % task_sequence_alg(Name),
    impls(AlgName, Alg),

    % impls('permutations', Alg),
    call(Alg, Robot, Tasks, Order).

sequence_algs(Algs) :- findall(Alg, impls(Alg, _), Algs).
