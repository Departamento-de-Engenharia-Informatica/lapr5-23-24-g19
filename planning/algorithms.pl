% vim: ft=prolog

:- module(algorithms, [
    criteria/1,
    compute_path/4
]).


criteria(['least-elevators', 'least-buildings']).


resolve_criterium('least-elevators', algorithms:crit1).
resolve_criterium('least-buildings', algorithms:crit2).

crit1(_TBD) :- fail.
crit2(_TBD) :- fail.



% Orig = (building, floor, x, y)
% Dest = (building, floor, x, y)
% Path: output
compute_path(Orig, Dest, Criterium, Path) :-
    resolve_criterium(Criterium, Crit),

    % some_alg(Orig, Dest, Crit, Path).

    fail.

