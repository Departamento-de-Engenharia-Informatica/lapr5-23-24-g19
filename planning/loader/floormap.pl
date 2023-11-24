:- module(floormap, [
    floorcell/4,
    elevator/5,
    passage/6,
    edge/5,
    loadmap/2
]).

% TODO: move to diff module
getmap(_Building, _Floor, _Map) :- fail.

:- dynamic floorcell/4.  % floorcell(building, floor, x, y)
:- dynamic elevator/5.   % elevator(building, floor, x, y, floors_covered)
:- dynamic passage/6.    % passage(b_orig, f_orig, x, y, b_dest, f_dest)
:- dynamic connection/5. % connection(Building, Floor, cell(X1, Y1), cell(X2, Y2), Cost)

edge(Building, Floor, E1, E2, Cost) :-
    connection(Building, Floor, C1, C2, Cost);
    connection(Building, Floor, C2, C1, Cost).

loadmap(Building, FLoor) :-
    getmap(Building, FLoor, Map),
    _{length: Len, width: Wid} :< Map.dimensions,

    populate_cells(Building, FLoor, Len, Wid),
    build_graph(Building, Floor, Map.mapContent),
    add_diagonals(Building, Floor),

    populate_passage(Building, Floor, Map.passages),
    populate_elev(Building, Floor, Map.elevators).



add_diagonals(Building, Floor) :-
    findall((X, Y), floorcell(Building, Floor, X, Y), Cells),
    add_diagonals_aux(Building, Floor, Cells).

add_diagonals_aux(_, _, []).
add_diagonals_aux(Building, Floor, [(X, Y)|Cells]) :-
    add_diagonal(Building, Floor, X, Y),
    add_diagonals_aux(Building, Floor, Cells).

add_diagonal(Building, Floor, X, Y) :-
    Xb is X - 1,
    Xa is X + 1,
    Yb is Y - 1,
    Ya is Y + 1,

    (% top left
        (
            connection(Building, Floor, cell(X, Y), cell(Xb, Y), _),
            connection(Building, Floor, cell(Xb, Y), cell(Xb, Yb), _),
            assertz(connection(Building, Floor, cell(X, Y), cell(Xb, Yb), sqrt(2)))
        ); true
    ),
    (% top right
        (
            connection(Building, Floor, cell(X, Y), cell(Xa, Y), _),
            connection(Building, Floor, cell(Xa, Y), cell(Xa, Yb), _),
            assertz(connection(Building, Floor, cell(X, Y), cell(Xa, Yb), sqrt(2)))
        ); true
    ),
    (% bottom left
        (
            connection(Building, Floor, cell(X, Y), cell(X, Ya), _),
            connection(Building, Floor, cell(X, Ya), cell(Xb, Ya), _),
            assertz(connection(Building, Floor, cell(X, Y), cell(Xb, Ya), sqrt(2)))
        ); true
    ),
    (% bottom right
        (
            connection(Building, Floor, cell(X, Y), cell(X, Ya), _),
            connection(Building, Floor, cell(X, Ya), cell(Xa, Ya), _),
            assertz(connection(Building, Floor, cell(X, Y), cell(Xa, Ya), sqrt(2)))
        ); true
    ).

build_graph(Building, Floor, FloorMap) :-
    length(FloorMap, Rows),
    [H|_] = FloorMap, length(H, Cols),

    Rows1 is Rows-1,
    Cols1 is Cols-1,

    build_graph_row(Building, Floor, FloorMap, Rows1, Cols1, 0, 0),

    add_diagonals(Building, Floor).

build_graph_row(_, _, [], _, _, _, _) :- !.
build_graph_row(Building, Floor, [Map|Maps], NRows, NCols, Row, Col) :-
    build_graph_col(Building, Floor, Map, NRows, NCols, Row, Col),
    Row1 is Row + 1,
    build_graph_row(Building, Floor, Maps, NRows, NCols, Row1, Col).

build_graph_col(_, _, [], _, _, _, _) :- !.
build_graph_col(Building, Floor, [Cell|Cells], NRows, NCols, Row, Col) :-
    Col1 is Col + 1,
    Row1 is Row + 1,
    (
        ((Cell \== 1); (Cell \== 3)),
        Col < NCols,
        assertz(connection(Building, Floor, cell(Row, Col), cell(Row,Col1), 1))
    ;   true
    ),
    (
        ((Cell \== 2); (Cell \== 3)),
        Row < NRows,
        assertz(connection(Building, Floor, cell(Row, Col, 1), cell(Row1,Col, 1), 1))
    ;   true
    ),
    build_graph_col(Building, Floor, Cells, NRows, NCols, Row, Col1).

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
