% vim: ft=prolog

:- module(task_sequence_dto, [
    sequence_to_dto/2,
    dto_to_sequence/2
]).


:- use_module('util/functional', [ map/3 ]).


sequence_to_dto((Cost, Order), DTO) :-
    map(task_sequence_dto:task_to_dto, Order, OrderDTO),

    DTO = _{
        cost: Cost,
        order: OrderDTO
    }.


dto_to_task(DTO, Task) :-
    Task = (
        start(
            DTO.start.building,
            DTO.start.floor,
            DTO.start.x,
            DTO.start.y
        ),
        end(
            DTO.end.building,
            DTO.end.floor,
            DTO.end.x,
            DTO.end.y
        ),
        DTO.type
    ).


task_to_dto((start(B1, F1, X1, Y1), end(B2, F2, X2, Y2), Type), DTO) :-
    DTO = _{
        type: Type,
        start: _{
            building: B1,
            floor: F1,
            x: X1,
            y: Y1
        },
        end: _{
            building: B2,
            floor: F2,
            x: X2,
            y: Y2
        }
    }.


dto_to_sequence(DTOList, Tasks) :- map(task_sequence_dto:dto_to_task, DTOList, Tasks).
