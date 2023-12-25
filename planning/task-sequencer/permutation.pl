:- module('permutation_sequencer', [
    perm/3
]).

:- use_module(library(lists), [ permutation/2 ]).

:- use_module('algorithms', [ compute_path/4 ]).


perm(Robot, Tasks, Order) :-
    findall(
        (Cost, TaskOrder),
        permutation_cost(Robot, Tasks, (Cost, TaskOrder)),
        Permutations
    ),
    sort(Permutations, Sorted),
    [Order|_] = Sorted.


permutation_cost(Robot, Tasks, (Cost, T)) :-
    % write('========= BEGIN PERMUTATION =========='), nl,
    permutation(Tasks, T),
    path_cost(Robot, T, Cost).
    % write('========= END PERMUTATION =========='), nl.


% TaskOrder = (start(B1, F1, X1, Y1), end(B2, F2, X2, Y2), TaskType)
path_cost(Robot, TaskOrder, Cost) :-
    % EXTRA: walk from initial position to first task
    [(start(B, F, X, Y), _, _) |_] = TaskOrder,
    compute_path(Robot, (B, F, X, Y), _, InitialCost), !,

    path_cost_aux(Robot, TaskOrder, InitialCost, Cost).


path_cost_aux(_, [], Acc, Acc).

path_cost_aux(Robot, [T], Acc, Cost) :-
    % EXTRA: walk from last task to initial position
    (_, end(B, F, X, Y), _) = T,
    % write('Computing path from '), write((B, F, X, Y)), write(' to '), write(Robot), nl,
    compute_path((B, F, X, Y), Robot, _, CostPath), !,
    % write('Cost: '), write(CostPath), nl,

    Cost is Acc + CostPath.

path_cost_aux(Robot, [T1, T2 | Tasks], Acc, Cost) :-
    (_, end(B1, F1, X1, Y1), _) = T1,
    (start(B2, F2, X2, Y2), _, _) = T2,

    % write('Computing path from '), write((B1, F1, X1, Y1)), write(' to '), write((B2, F2, X2, Y2)), nl,
    compute_path((B1, F1, X1, Y1), (B2, F2, X2, Y2), _, CostPath), !,
    % write('Cost: '), write(CostPath), nl,
    NewAcc is Acc + CostPath,

    path_cost_aux(Robot, [T2|Tasks], NewAcc, Cost).

% vim: ft=prolog
