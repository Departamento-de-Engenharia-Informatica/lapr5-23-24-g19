% vim: ft=prolog

:- module(algorithms, [
    criteria/1,
    compute_paths/4
]).


:- use_module(library(lists)).
:- use_module(library(sort)).

:- use_module('util/functional', [ map/3, compare_on/4 ]).
:- use_module('loader/floormap', [ loadmap/2 ]).

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


% TODO: somehow create a wrapper around these
% in order to avoid calling loadmap/2 on each one
% while also avoiding mutual recursion
some_alg((B, F, X, Y), (B, F, X, Y), []).
some_alg((B, F, X1, X2), (B, F, X1, X2), Path) :-
    % walk the floor until we reach the goal
    fail.
some_alg((B, F1, X1, Y1), (B, F2, X2, Y2), Path) :-
    % walk the floor until we reach the elevator
    % catch the elevator to another floor
    fail.
some_alg((B1, F1, X1, Y1), (B2, F2, X2, Y2), Path) :-
    % walk the floor until we reach the passage
    % go to the other building
    % use passage/6 to find where we end up in
    fail.


% Orig = (building, floor, x, y)
% Dest = (building, floor, x, y)
% Paths: output
compute_paths(Orig, Dest, Criterium, Paths) :-
    findall(Ligs, some_alg(Orig, Dest, Ligs), LLigs),

    % pick criterium
    resolve_criterium(Criterium, Crit),

    predsort(Crit, Ligs, Paths).


