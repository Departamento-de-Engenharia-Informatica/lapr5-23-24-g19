% vim: ft=prolog

:- module(algorithms, [
    criteria/1,
    compute_paths/4
]).


:- use_module(library(lists)).
:- use_module(library(sort)).

:- use_module('util/functional', [ map/3, compare_on/4 ]).
:- use_module('loader/floormap', [ loadmap/2 ]).

:- use_module('graph', [
    floorcell/4,
    elevator/5,
    passage/6,
    edge/5
]).

:- use_module('algorithms/a-star', [ a_star/5 as walk ]).


% NOTE: Notation used for paths:
% Path = [PathElement]
% PathElement = cell(building, floor, x, y)
%             | elev(b, f1, f2)
%             | pass(b1, f1, b2, f2).

% compute all available criterion
criteria(Crits) :- findall(Crit, resolve_criterion(Crit, _), Crits).


resolve_criterion('least-elevators', algorithms:less_elevators).
resolve_criterion('least-buildings', algorithms:less_buildings).


% TODO: implement each criteria on it's own file & import it?
less_elevators(Sign, Path1, Path2) :- compare_on(algorithms:count_elev, Path1, Path2, Sign).
less_buildings(Sign, Path1, Path2) :- compare_on(algorithms:count_build, Path1, Path2, Sign).



count_elev(Xs, Count) :- count_elev_aux(Xs, 0, Count).
count_elev_aux([], Acc, Acc).
count_elev_aux([elev(_, _, _)|Xs], Acc, Count) :- !,
    NewAcc is Acc + 1,
    count_elev_aux(Xs, NewAcc, Count).
count_elev_aux([_|Xs], Acc, Count) :- count_elev_aux(Xs, Acc, Count).


% not sure if the best impl
count_build(Path, Count) :-
    map(algorithms:extract_build, Path, Builds),
    flatten(Builds, FlatBuilds),
    sort(FlatBuilds, Dedups), % HACK: we're sorting only to remove duplicates
    length(Dedups, Count).

extract_build(elev(B, _, _), B).
extract_build(pass(B1, _, B2, _), [B1, B2]).
extract_build(cell(B, _, _, _), B).


% Orig = (building, floor, x, y)
% Dest = (building, floor, x, y)
% Paths: output
compute_paths(Orig, Dest, Criterion, Paths) :-
    % TODO: environment variable
    findnsols(30, Ligs, compute_path(Orig, Dest, Ligs), LLigs),

    % pick criterion
    resolve_criterion(Criterion, Crit),
    predsort(Crit, LLigs, Paths).

compute_path(Orig, Dest, Path) :-
    compute_path_aux(Orig, Dest, NestedPath),
    flatten(NestedPath, Path).


cell_to_floorcell(Building, Floor, Cell, FloorCell) :-
    Cell = cell(X, Y),
    FloorCell = cell(Building, Floor, X, Y).
absolute_path(Building, Floor, Relative, Absolute) :-
    map(algorithms:cell_to_floorcell(Building, Floor), Relative, Absolute).
edge_wrap(B, F, {B,F}/[C1, C2, Cost]>>(graph:edge(B, F, C1, C2, Cost))).



% FIXME: accumutate Cost

compute_path_aux((B, F, X, Y), (B, F, X, Y), []):- !.
% same building, same floor
compute_path_aux((B, F, X1, Y1), (B, F, X2, Y2), Path) :-
    loadmap(B, F),

    % lambda wrapper around edge
    edge_wrap(B, F, Wrapper),

    walk(cell(X1, Y1), cell(X2, Y2), Wrapper, PathRelative, _Cost),
    absolute_path(B, F, PathRelative, Path).

% same building, different floor (catch elevator)
compute_path_aux((B, F1, X1, Y1), (B, F2, X2, Y2), [CompFull|Path]) :-
    loadmap(B, F1),

    elevator(B, F1, XElev, YElev, ElevFloors),
    member(F2, ElevFloors),

    edge_wrap(B, F1, Wrapper),

    % walk the floor until we reach the elevator
    walk(cell(X1, Y1), cell(XElev, YElev), Wrapper, Component, _Cost),
    absolute_path(B, F1, Component, CompAbs),

    % catch the elevator to another floor
    % FIXME: find coords of elevator in the other floor
    Xf2 = XElev, Yf2 = YElev,

    append(CompAbs, [elev(B, F1, F2)], CompFull),

    compute_path_aux((B, F2, Xf2, Yf2), (B, F2, X2, Y2), Path).

% different building, different floor
% try passage to B2
compute_path_aux((B1, F1, X1, Y1), (B2, F2, X2, Y2), [CompFull|Path]) :-
    loadmap(B1, F1),
    % walk the floor until we reach the passage
    % go to the other building
    % use passage/6 to find where we end up in

    passage(B1, F1, Xb1, Yb1, B2, Fb2),

    edge_wrap(B1, F1, Wrapper),

    walk(cell(X1, Y1), cell(Xb1, Yb1), Wrapper, Component, _Cost),
    absolute_path(B1, F1, Component, CompAbs),

    append(CompAbs, [pass(B1, F2, B2, F2)], CompFull),

    % get coords from the other side, Xb2 & Yb2
    loadmap(B2, Fb2),
    passage(B2, Fb2, Xb2, Yb2, B1, F1),
    compute_path_aux((B2, Fb2, Xb2, Yb2), (B2, F2, X2, Y2), Path).


% different building, different floor
% try random elevator
compute_path_aux((B1, F1, X1, Y1), (B2, F2, X2, Y2), [CompFull|Path]) :-
    elevator(B1, F1, XElev, YElev, ElevFloors),
    member(Fconn, ElevFloors),

    edge_wrap(B1, F1, Wrapper),
    walk(cell(X1, Y1), cell(XElev, YElev), Wrapper, Comp, _Cost),
    absolute_path(B1, F1, Comp, CompAbs),

    % catch the elevator to another floor
    % FIXME: find coords of elevator in the other floor
    Xf2 = XElev, Yf2 = YElev,

    append(CompAbs, [elev(B1, F1, Fconn)], CompFull),
    compute_path_aux((B1, Fconn, Xf2, Yf2), (B2, F2, X2, Y2), Path).


% different building, different floor
% try random passage
compute_path_aux((B1, F1, X1, Y1), (B2, F2, X2, Y2), [CompFull|Path]) :-
    loadmap(B1, F1),

    passage(B1, F1, Xf1, Yf1, B3, F3),

    edge_wrap(B1, F1, Wrapper),
    walk(cell(X1, Y1), cell(Xf1, Yf1), Wrapper, Comp, _Cost),
    absolute_path(B1, F1, Comp, CompAbs),

    append(CompAbs, [pass(B1, F1, B3, F3)], CompFull),

    loadmap(B3, F3),
    passage(B3, F3, Xb3, Yb3, B1, F1),
    compute_path_aux((B3, F3, Xb3, Yb3), (B2, F2, X2, Y2), Path).


