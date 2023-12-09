:- module('permutation_sequencer', [
    perm/2
]).

:- use_module(library(lists), [ permutation/2 ]).

:- use_module('algorithms', [ compute_path/4 ]).


perm(Tasks, Order) :-
    findall(
        (Cost, TaskOrder),
        permutation_cost(Tasks, (Cost, TaskOrder)),
        Permutations
    ),
    sort(Permutations, Sorted),
    [Order|_] = Sorted.


permutation_cost(Tasks, (Cost, T)) :-
    permutation(Tasks, T),
    path_cost(T, Cost).


% TaskOrder = (start(B1, F1, X1, Y1), end(B2, F2, X2, Y2), Task)
path_cost(TaskOrder, Cost) :-
    % EXTRA: walk from initial position to first task
    path_cost_aux(TaskOrder, 0, Cost).


path_cost_aux([], Acc, Acc).
path_cost_aux([_], Acc, Acc). % EXTRA: walk from last task to initial position

path_cost_aux([T1, T2 | Tasks], Acc, Cost) :-
    (_, end(B1, F1, X1, Y1), _) = T1,
    (start(B2, F2, X2, Y2), _, _) = T2,

    compute_path((B1, F1, X1, Y1), (B2, F2, X2, Y2), _, CostPath), !,
    NewAcc = Acc + CostPath,

    path_cost_aux([T2|Tasks], NewAcc, Cost).

% vim: ft=prolog
