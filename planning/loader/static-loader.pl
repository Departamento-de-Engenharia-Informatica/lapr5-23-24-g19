:- module('static-loader', [
    getmap/3
]).

:- dynamic loaded/3. % loaded(Building, Floor, UNIX_time)

:- use_module(graph, [
    floorcell/4,
    elevator/5,
    passage/6,
    connection/5
]).

maps('K', 2, _{
    dimensions: _{ length: 26, width: 12 },
    mapContent: [
        [3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1],
        [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
        [3, 2, 1, 0, 0, 0, 3, 2, 2, 2, 0, 2, 1],
        [1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1],
        [3, 2, 2, 2, 1, 0, 3, 2, 2, 2, 2, 2, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 3, 2, 2, 2, 2, 2, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [3, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 3, 2, 2, 2, 2, 2, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1],
        [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1]
    ],
    passages: [
        _{
            x: 2,
            y: 1,
            orientation: 'N',
            to: _{
                building: 'E',
                floor: 3
            }
        },
        _{
            x: 5,
            y: 8,
            orientation: 'E',
            to: _{
                building: 'A',
                floor: 1
            }
        }
    ],
    elevators: [
        _{
            x: 2,
            y: 8,
            orientation: 'E',
            floors: [1, 3, 4, 5, 6]
        }
    ]
}).

getmap(Building, Floor, Map) :-
    maps(Building, Floor, Map),

    (
        (retractall(floorcell(Building, Floor, _, _)), !; true),
        (retractall(elevator(Building, Floor, _, _, _)), !; true),
        (retractall(passage(Building, Floor, _, _, _, _)), !; true),
        (retractall(connection(Building, Floor, _, _, _)), !; true),
        (retractall(room(Building, Floor, _, _, _)), !; true)
    ).

% vim: ft=prolog

% vim: ft=prolog
