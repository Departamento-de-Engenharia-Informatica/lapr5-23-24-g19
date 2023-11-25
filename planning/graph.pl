:- module(graph, [
    floorcell/4,
    elevator/5,
    passage/6,
    connection/5,
    edge/5
]).


:- dynamic floorcell/4.  % floorcell(building, floor, x, y)
:- dynamic elevator/5.   % elevator(building, floor, x, y, floors_covered)
:- dynamic passage/6.    % passage(b_orig, f_orig, x, y, b_dest, f_dest)
:- dynamic connection/5. % connection(Building, Floor, cell(X1, Y1), cell(X2, Y2), Cost)

edge(Building, Floor, C1, C2, Cost) :-
    connection(Building, Floor, C1, C2, Cost);
    connection(Building, Floor, C2, C1, Cost).

% vim: ft=prolog
