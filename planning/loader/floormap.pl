:- module(floormap, [
    loadmap/2
]).

:- use_module('loader/http-loader', [ getmap/3 ]).

:- use_module('graph', [
    floorcell/4,
    elevator/5,
    room/5,
    passage/6,
    connection/5,
    edge/5,
    wipe_floor/2
]).

loadmap(Building, Floor) :-
    wipe_floor(Building, Floor),
    getmap(Building, Floor, Map),

    _{length: Len, width: Wid} :< Map.dimensions,

    populate_cells(Building, Floor, Len, Wid),

    populate_rooms(Building, Floor, Map.rooms),
    remove_door_cells(Building, Floor),

    build_graph(Building, Floor, Map.mapContent),
    add_diagonals(Building, Floor),

    populate_passage(Building, Floor, Map.passages),
    populate_elev(Building, Floor, Map.elevators),
    !.


populate_cells(_, _, 0, _) :- !.
populate_cells(Building, Floor, Y, X) :-
    Y1 is Y-1,
    populate_cells_aux(Building, Floor, Y1, X),
    populate_cells(Building, Floor, Y1, X).

populate_cells_aux(_, _, _, 0) :- !.
populate_cells_aux(Building, Floor, Y, X) :-
    X1 is X-1,
    asserta(floorcell(Building, Floor, X1, Y)),
    populate_cells_aux(Building, Floor, Y, X1).


build_graph(Building, Floor, FloorMap) :-
    length(FloorMap, Rows),
    [H|_] = FloorMap, length(H, Cols),

    Rows1 is Rows-1,
    Cols1 is Cols-1,

    build_graph_row(Building, Floor, FloorMap, Rows1, Cols1, 0, 0).

build_graph_row(_, _, [], _, _, _, _) :- !.
build_graph_row(Building, Floor, [Map|Maps], NRows, NCols, Row, Col) :-
    build_graph_col(Building, Floor, Map, NRows, NCols, Row, Col),
    Row1 is Row + 1,
    build_graph_row(Building, Floor, Maps, NRows, NCols, Row1, Col).

build_graph_col(_, _, [], _, _, _, _) :- !.
build_graph_col(Building, Floor, [Cell|Cells], NRows, NCols, Y, X) :-
    X1 is X - 1,
    Y1 is Y - 1,
    (% connect to the left
        ((Cell \== 1), (Cell \== 3)),
        X < NCols,
        floorcell(Building, Floor, X1, Y),
        assertz(connection(Building, Floor, cell(X, Y), cell(X1, Y), 1))
    ;   true
    ),
    (% connect above
        ((Cell \== 2), (Cell \== 3)),
        Y < NRows,
        floorcell(Building, Floor, X, Y1),
        assertz(connection(Building, Floor, cell(X, Y), cell(X, Y1), 1))
    ;   true
    ),

    Row is X + 1,
    build_graph_col(Building, Floor, Cells, NRows, NCols, Y, Row).


add_diagonals(Building, Floor) :-
    findall((X, Y), floorcell(Building, Floor, X, Y), Cells),
    add_diagonals_aux(Building, Floor, Cells).

add_diagonals_aux(_, _, []).
add_diagonals_aux(Building, Floor, [(X, Y)|Cells]) :-
    add_diagonal(Building, Floor, X, Y),
    add_diagonals_aux(Building, Floor, Cells).

add_diagonal(Building, Floor, X, Y) :-
    Xb is X - 1, Xa is X + 1,
    Yb is Y - 1, Ya is Y + 1,

    (add_diagonal_aux(Building, Floor, X, Y, Xb, Yb); true),
    (add_diagonal_aux(Building, Floor, X, Y, Xb, Ya); true),
    (add_diagonal_aux(Building, Floor, X, Y, Xa, Yb); true),
    (add_diagonal_aux(Building, Floor, X, Y, Xa, Ya); true).

add_diagonal_aux(Building, Floor, X_orig, Y_orig, X_dest, Y_dest) :-
    (
        (
            (
                edge(Building, Floor, cell(X_orig, Y_orig), cell(X_dest, Y_orig), _),
                edge(Building, Floor, cell(X_dest, Y_orig), cell(X_dest, Y_dest), _)
            ), !
        );
        (
            edge(Building, Floor, cell(X_orig, Y_orig), cell(X_orig, Y_dest), _),
            edge(Building, Floor, cell(X_orig, Y_dest), cell(X_dest, Y_dest), _)
        )
    ),
    assertz(connection(Building, Floor, cell(X_orig, Y_orig), cell(X_dest, Y_dest), sqrt(2))).


populate_passage(_, _, []).
populate_passage(Building, Floor, [P|Ps]) :-
    _{ x:Y, y:X } :< P, % NOTE: X & Y flipped is not a bug
    _{ building: BDestStr, floor: FDest } :< P.to,
    atom_string(BDest, BDestStr),

    assertz(passage(Building, Floor, X, Y, BDest, FDest)),
    populate_passage(Building, Floor, Ps).

populate_elev(_, _, []).
populate_elev(Building, Floor, [E|Es]) :-
    _{ x:Y, y:X, floors: Fs } :< E, % NOTE: X & Y flipped is not a bug
    assertz(elevator(Building, Floor, X, Y, Fs)),
    populate_elev(Building, Floor, Es).

populate_rooms(_, _, []).
populate_rooms(Building, Floor, [R|Rs]) :-
    _{ x:Y, y:X, orientation:O } :< R, % NOTE: X & Y flipped is not a bug
    atom_string(Orientation, O),

    assertz(room(Building, Floor, X, Y, Orientation)),
    populate_rooms(Building, Floor, Rs).

remove_door_cells(Building, Floor) :-
    forall(
        room(Building, Floor, X, Y, _),
        (retract(floorcell(Building, Floor, X, Y)), !; true)
    ).


% vim: ft=prolog
