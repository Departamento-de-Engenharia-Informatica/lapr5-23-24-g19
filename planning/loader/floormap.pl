:- module(floormap, [
    floorcell/4,
    elevator/5,
    passage/6,
    loadmap/2
]).

% TODO: move to diff module
getmap(_Building, _Floor, _Map) :- fail.

:- dynamic floorcell/4. % floorcell(building, floor, x, y)
:- dynamic elevator/5.  % elevator(building, floor, x, y, floors_covered)
:- dynamic passage/6.   % passage(b_orig, f_orig, x, y, b_dest, f_dest)

loadmap(Building, FLoor) :-
    getmap(Building, FLoor, Map),
    _{length: Len, width: Wid} = Map.dimensions,
    populate_cells(Building, FLoor, Len, Wid),

    populate_passage(Building, Floor, Map.passages),
    populate_elev(Building, Floor, Map.elevators),

    % TODO: add connections
    fail.


populate_cells(_, _, 0, _) :- !.
populate_cells(Building, FLoor, X, Y) :-
    X1 is X-1,
    populate_cells_aux(Building, FLoor, X1, Y),
    populate_cells(Building, FLoor, X1, Y).

populate_cells_aux(_, _, _, 0) :- !.
populate_cells_aux(Building, FLoor, X, Y) :-
    Y1 is Y-1,
    assertz(floorcell(Building, FLoor, X, Y1)),
    populate_cells_aux(Building, FLoor, X, Y1).


populate_elev(_, _, []).
populate_elev(Building, Floor, [E|Es]) :-
    _{ x:X, y:Y, floors: Fs } :< E,
    assertz(elevator(Building, Floor, X, Y, Fs)),
    populate_elev(Building, Floor, Es).


populate_passage(_, _, []).
populate_passage(Building, Floor, [P|Ps]) :-
    _{ x:X, y:Y } :< P,
    _{ building: BDest, floor: FDest } :< P.to,

    assertz(passage(Building, Floor, X, Y, BDest, FDest)),
    populate_passage(Building, Floor, Ps).

% vim: ft=prolog
