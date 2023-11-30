:- module(path_segment_dto, [
    segments_to_dto/2
]).

:- use_module('util/functional', [ map/3 ]).

dto_single(cell(B, F, X, Y), Dto) :-
    Dto = _{
        building: B,
        floor: F,
        x: X,
        y: Y,
        type: 'cell'
    }.
dto_single(elev(B, F1, F2), Dto) :-
    Dto = _{
        building: B,
        fromFloor: F1,
        toFloor: F2,
        type: 'elevator'
    }.
dto_single(pass(B1, F1, B2, F2), Dto) :-
    Dto = _{
        fromBuilding: B1,
        fromFloor: F1,
        toBuilding: B2,
        toFloor: F2,
        type: 'passage'
    }.

segments_to_dto(Segs, Dtos) :- map(path_segment_dto:dto_single, Segs, Dtos).

% vim: ft=prolog
