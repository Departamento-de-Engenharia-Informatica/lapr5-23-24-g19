% vim: ft=prolog

:- module(algorithms, [
    criteria/1,
    compute_paths/4
]).


:- use_module(library(sort)).

% NOTE: Notation used for paths:
% Path = [PathElement]
% PathElement = elev(building, floor1, floor2)
%             | pass(b1, f1, b2, f2).

resolve_criterium('least-buildings', algorithms:crit2).
% compute all available criterium
criteria(Crits) :- findall(Crit, resolve_criterium(Crit, _), Crits).

crit2(_TBD) :- fail.

resolve_criterium('least-elevators', algorithms:less_elevators).


less_elevators(Sign, Path1, Path2) :- compare_on(count_elev, Path1, Path2, Sign).
% TODO: move to a utils/functional.pl file
compare_on(Func, X, Y, Sign) :-
    call(Func, X, X1),
    call(Func, Y, Y1),
    compare(Sign, X1, Y1).

count_elev(Xs, Count) :- count_elev_aux(Xs, 0, Count).
count_elev_aux([], Acc, Acc).
count_elev_aux([elev(_, _, _)|Xs], Acc, Count) :- !,
    NewAcc is Acc + 1,
    count_elev_aux(Xs, NewAcc, Count).
count_elev_aux([_|Xs], Acc, Count) :- count_elev_aux(Xs, Acc, Count).




some_alg(_Orig, _Dest, _Path) :- fail. % TODO

% Orig = (building, floor, x, y)
% Dest = (building, floor, x, y)
% Paths: output
compute_paths(Orig, Dest, Criterium, Paths) :-
    findall(Ligs, some_alg(Orig, Dest, Ligs), LLigs),

    % pick criterium
    resolve_criterium(Criterium, Crit),

    predsort(Crit, Ligs, Paths).


