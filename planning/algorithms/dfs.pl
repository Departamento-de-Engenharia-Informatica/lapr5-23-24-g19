:- module(algorithms_dfs, [
    dfs/5
]).

dfs(Orig, Dest, EdgePred, Path, Cost) :-
    dfs_aux(Orig, Dest, EdgePred, [Orig], 0, Path, Cost).


dfs_aux(Dest, Dest, _, LA, Acc, Path, Acc) :-
    reverse(LA, Path).

dfs_aux(Act, Dest, EdgePred, LA, Acc, Path, Cost) :-
    call(EdgePred, Act, X, CostX),
    \+ member(X, LA),
    NewAcc is Acc + CostX,
    dfs_aux(X, Dest, EdgePred, [X|LA], NewAcc, Path, Cost).

% vim: ft=prolog
