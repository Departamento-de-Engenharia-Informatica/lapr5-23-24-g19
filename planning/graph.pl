:- module(graph, [
    floorcell/4,
    elevator/5,
    passage/6,
    connection/5,
    room/5,

    edge/5,
    resolve_room/2,
    wipe_floor/2
]).


:- dynamic floorcell/4.  % floorcell(building, floor, x, y)
:- dynamic elevator/5.   % elevator(building, floor, x, y, floors_covered)
:- dynamic passage/6.    % passage(b_orig, f_orig, x, y, b_dest, f_dest)
:- dynamic connection/5. % connection(Building, Floor, cell(X1, Y1), cell(X2, Y2), Cost)
:- dynamic room/5.       % room(building, floor, door_x, door_y, 'N'|'S'|'W'|'E').


edge(Building, Floor, C1, C2, Cost) :-
    connection(Building, Floor, C1, C2, Cost);
    connection(Building, Floor, C2, C1, Cost).


resolve_room((Building, Floor, X, Y), Act) :-
    room(Building, Floor, X, Y, Orientation),
    resolve_room_aux(X, Y, Orientation, ActX, ActY),
    Act = (Building, Floor, ActX, ActY).

resolve_room_aux(X, Y, 'N', X, Y1) :- Y1 is Y-1.
resolve_room_aux(X, Y, 'S', X, Y1) :- Y1 is Y+1.

resolve_room_aux(X, Y, 'W', X1, Y) :- X1 is X-1.
resolve_room_aux(X, Y, 'E', X1, Y) :- X1 is X+1.


wipe_floor(Building, Floor) :-
    (retractall(floorcell(Building, Floor, _, _)), !; true),
    (retractall(elevator(Building, Floor, _, _, _)), !; true),
    (retractall(passage(Building, Floor, _, _, _, _)), !; true),
    (retractall(connection(Building, Floor, _, _, _)), !; true),
    (retractall(room(Building, Floor, _, _, _)), !; true).

% vim: ft=prolog
