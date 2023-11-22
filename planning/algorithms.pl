% vim: ft=prolog

:- module(algorithms, [
    criteria/1,
    compute_paths/4
]).


:- use_module(library(lists)).
:- use_module(library(sort)).

% NOTE: Notation used for paths:
% Path = [PathElement]
% PathElement = elev(building, floor1, floor2)
%             | pass(b1, f1, b2, f2).

% compute all available criterium
criteria(Crits) :- findall(Crit, resolve_criterium(Crit, _), Crits).


resolve_criterium('least-elevators', algorithms:less_elevators).
resolve_criterium('least-buildings', algorithms:less_buildings).


% TODO: implement each criteria on it's own file & import it?

less_elevators(Sign, Path1, Path2) :- compare_on(count_elev, Path1, Path2, Sign).
less_buildings(Sign, Path1, Path2) :- compare_on(count_build, Path1, Path2, Sign).

% TODO: move to a utils/functional.pl file
map(_, [], []).
map(Func, [X|Xs], [Y|Ys]) :-
    call(Func, X, Y),
    map(Func, Xs, Ys).

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


% not sure if the best impl
count_build(Path, Count) :-
    map(extract_build, Path, Builds),
    flatten(Builds, FlatBuilds),
    sort(FlatBuilds, Dedups), % HACK: we're sorting only to remove duplicates
    length(Dedups, Count).

extract_build(elev(B, _, _), B).
extract_build(pass(B1, _, B2, _), [B1, B2]).



some_alg(_Orig, _Dest, _Path) :- fail. % TODO

% Orig = (building, floor, x, y)
% Dest = (building, floor, x, y)
% Paths: output
compute_paths(Orig, Dest, Criterium, Paths) :-
    findall(Ligs, some_alg(Orig, Dest, Ligs), LLigs),

    % pick criterium
    resolve_criterium(Criterium, Crit),

    predsort(Crit, Ligs, Paths).


