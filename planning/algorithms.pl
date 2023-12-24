% vim: ft=prolog

:- module(algorithms, [
    criteria/1,
    compute_paths/4,
    compute_path/4
]).


:- use_module(library(lists)).
:- use_module(library(sort)).

:- use_module('util/functional', [ map/3, compare_on/4 ]).
:- use_module('loader/floormap', [ loadmap/2 ]).

:- use_module('env', [ passage_cost/1, elev_cost/1 ]).

:- use_module('graph', [
    floorcell/4,
    elevator/5,
    passage/6,
    edge/5,
    resolve_room/2
]).

% :- use_module('algorithms/a-star', [ a_star/5 as walk ]).
% :- use_module('algorithms/dfs', [ dfs/5 as walk ]).
% :- use_module('algorithms/best-first', [ bestfs/5 as walk ]).
:- use_module('algorithms/beam', [ beam/5 as walk ]).


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
    % pick criterion
    resolve_criterion(Criterion, Crit),

    % TODO: environment variable
    findnsols(30, Ligs, compute_path(Orig, Dest, Ligs, _Cost), LLigs),

    predsort(Crit, LLigs, Paths).

:- dynamic visited_floors/2.
compute_path(Orig, Dest, Path, Cost) :-
    % write('oi'), nl,

    (retractall(visited_floors(_, _)), !; true),

    (BOrig, FOrig, _, _) = Orig,
    (BDest, FDest, _, _) = Dest,

    loadmap(BOrig, FOrig),
    loadmap(BDest, FDest),

    (resolve_room(Orig, ActOrig), !; ActOrig = Orig),
    (resolve_room(Dest, ActDest), !; ActDest = Dest),

    % write('orig: '), write(Orig), nl,
    % write('origem: '), write(ActOrig), nl,
    % write('dest: '), write(Dest), nl,
    % write('destino: '), write(ActDest), nl,

    compute_path_aux(ActOrig, ActDest, NestedPath, 0, Cost),
    flatten(NestedPath, Path).


cell_to_floorcell(Building, Floor, Cell, FloorCell) :-
    Cell = cell(X, Y),
    FloorCell = cell(Building, Floor, X, Y).
absolute_path(Building, Floor, Relative, Absolute) :-
    map(algorithms:cell_to_floorcell(Building, Floor), Relative, Absolute).
edge_wrap(B, F, {B,F}/[C1, C2, Cost]>>(graph:edge(B, F, C1, C2, Cost))).


compute_path_aux((B, F, X, Y), (B, F, X, Y), [], Acc, Acc):- !.
% same building, same floor
compute_path_aux((B, F, X1, Y1), (B, F, X2, Y2), Path, Acc, Cost) :- !,
    loadmap(B, F),
    % format('~w~w (~w, ~w)', [B, F, X1, Y1]), nl,

    % lambda wrapper around edge
    edge_wrap(B, F, Wrapper),

    % write('comecou'), nl,
    walk(cell(X1, Y1), cell(X2, Y2), Wrapper, PathRelative, CostPath),
    % write('acabou'), nl,

    Cost is Acc + CostPath,

    absolute_path(B, F, PathRelative, Path).

% same building, different floor (catch elevator)
compute_path_aux((B, F1, X1, Y1), (B, F2, X2, Y2), [CompFull|Path], Acc, Cost) :-
    loadmap(B, F1),
    % format('~w~w (~w, ~w)', [B, F1, X1, Y1]), nl,

    elevator(B, F1, XElev, YElev, ElevFloors),
    member(F2, ElevFloors),

    edge_wrap(B, F1, Wrapper),

    % walk the floor until we reach the elevator
    % write('comecou'), nl,
    % write('same building elevator'), nl,
    walk(cell(X1, Y1), cell(XElev, YElev), Wrapper, Component, CostPath),
    % write('finished same building elevator'), nl,
    % write('acabou'), nl,
    absolute_path(B, F1, Component, CompAbs),

    % catch the elevator to another floor
    % FIXME: find coords of elevator in the other floor
    Xf2 = XElev, Yf2 = YElev,

    elev_cost(ElevCost),
    NewAcc is Acc + CostPath + ElevCost,

    append(CompAbs, [elev(B, F1, F2)], CompFull),
    assertz(visited_floors(B, F1)),

    compute_path_aux((B, F2, Xf2, Yf2), (B, F2, X2, Y2), Path, NewAcc, Cost).

% different building, different floor
% try passage to B2
compute_path_aux((B1, F1, X1, Y1), (B2, F2, X2, Y2), [CompFull|Path], Acc, Cost) :-
    \+ visited_floors(B1, F1),
    loadmap(B1, F1),
    % walk the floor until we reach the passage
    % go to the other building
    % use passage/6 to find where we end up in
    % format('~w~w (~w, ~w)', [B1, F1, X1, Y1]), nl,

    passage(B1, F1, Xb1, Yb1, B2, Fb2),

    edge_wrap(B1, F1, Wrapper),

    % write('passage to other building'), nl,
    walk(cell(X1, Y1), cell(Xb1, Yb1), Wrapper, Component, CostPath),
    % format('From ~w~w to ~w~w', [B1, F1, B2, Fb2]), nl,
    absolute_path(B1, F1, Component, CompAbs),

    append(CompAbs, [pass(B1, F2, B2, F2)], CompFull),

    % get coords from the other side, Xb2 & Yb2
    loadmap(B2, Fb2),
    passage(B2, Fb2, Xb2, Yb2, B1, F1),

    passage_cost(PassCost),
    NewAcc is Acc + CostPath + PassCost,

    assertz(visited_floors(B1, F1)),

    compute_path_aux((B2, Fb2, Xb2, Yb2), (B2, F2, X2, Y2), Path, NewAcc, Cost).

% different building, different floor
% try random passage
compute_path_aux((B1, F1, X1, Y1), (B2, F2, X2, Y2), [CompFull|Path], Acc, Cost) :-
    \+ visited_floors(B1, F1),
    loadmap(B1, F1),
    % format('~w~w (~w, ~w)', [B1, F1, X1, Y1]), nl,
    % write('random building passage'), nl,

    passage(B1, F1, Xf1, Yf1, B3, F3),
    \+ visited_floors(B3, F3),

    edge_wrap(B1, F1, Wrapper),
    walk(cell(X1, Y1), cell(Xf1, Yf1), Wrapper, Comp, CostPath),
    absolute_path(B1, F1, Comp, CompAbs),

    append(CompAbs, [pass(B1, F1, B3, F3)], CompFull),

    loadmap(B3, F3),
    passage(B3, F3, Xb3, Yb3, B1, F1),

    passage_cost(PassCost),
    NewAcc is Acc + CostPath + PassCost,

    assertz(visited_floors(B1, F1)),

    compute_path_aux((B3, F3, Xb3, Yb3), (B2, F2, X2, Y2), Path, NewAcc, Cost).

% different building, different floor
% try random elevator
compute_path_aux((B1, F1, X1, Y1), (B2, F2, X2, Y2), [CompFull|Path], Acc, Cost) :-
    \+ visited_floors(B1, F1),
    loadmap(B1, F1),
    % format('~w~w (~w, ~w)', [B1, F1, X1, Y1]), nl,
    % write('random building elevator'), nl,

    elevator(B1, F1, XElev, YElev, ElevFloors),
    member(Fconn, ElevFloors),
    Fconn \== F1,
    \+ visited_floors(B1, Fconn),
    % write(Fconn), nl,

    edge_wrap(B1, F1, Wrapper),
    % write('random building elevator'), nl,
    walk(cell(X1, Y1), cell(XElev, YElev), Wrapper, Comp, CostPath),
    % write('finshed random building elevator'), nl,
    absolute_path(B1, F1, Comp, CompAbs),

    % catch the elevator to another floor
    % FIXME: find coords of elevator in the other floor
    Xf2 = XElev, Yf2 = YElev,

    elev_cost(ElevCost),
    NewAcc is Acc + CostPath + ElevCost,

    append(CompAbs, [elev(B1, F1, Fconn)], CompFull),
    assertz(visited_floors(B1, F1)),

    compute_path_aux((B1, Fconn, Xf2, Yf2), (B2, F2, X2, Y2), Path, NewAcc, Cost).
