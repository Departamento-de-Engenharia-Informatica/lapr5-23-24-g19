% vim: ft=prolog

:- module('http-server', [
    create_server/1,
    stop_server/0
]).


:- use_module(library(http/http_cors)).
:- use_module(library(http/http_client)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_unix_daemon)).

:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).

:- use_module(library(settings)).

:- use_module(library(dicts)).


:- use_module('util/functional', [ map/3 ]).

:- use_module(algorithms, [
    criteria/1,
    compute_paths/4
]).

:- use_module('task-sequencer/sequencer', [ sequencer/2 ]).

:- use_module('dto/path-segment', [ segments_to_dto/2 ]).
:- use_module('dto/task-sequence', [ sequence_to_dto/2, dto_to_sequence/2 ]).

%%%%%%

:- set_setting(http:cors, [*]).

create_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    asserta(port(Port)).

stop_server :-
    retract(port(Port)),
    http_stop_server(Port, _).

%% Routes

:- http_handler('/api/comm-test', comm_test, [ method(get) ]).

comm_test(_Request) :- format('Status: 200~n~n').

:- http_handler('/api/paths/criteria', path_criteria, [ method(get) ]).

path_criteria(_Request) :-
    criteria(Crit),
    prolog_to_json(Crit, JSON),
    reply_json(JSON).


% should be a /GET
% method(post) does not work
:- http_handler('/api/paths', get_paths, [method(*)]).

get_paths(Request) :-
    http_read_json_dict(Request, Body),

    Start = Body.start,
    Goal = Body.goal,
    atom_string(Crit, Body.criteria),

    Orig = (Start.building, Start.floor, Start.coordinates.x, Start.coordinates.y),
    Dest = (Goal.building, Goal.floor, Goal.coordinates.x, Goal.coordinates.y),

    compute_paths(Orig, Dest, Crit, Paths),
    map(path_segment_dto:segments_to_dto, Paths, PathsDTO),

    reply_json_dict(PathsDTO).



:- use_module('task-sequencer/permutation', [ perm/2 ]).

:- http_handler('/api/task-sequence', get_sequence, [method(*)]).

get_sequence(Request) :-
    http_read_json_dict(Request, Body),

    dto_to_sequence(Body.tasks, Tasks),

    % sequencer(Tasks, Order),
    perm(Tasks, Order),
    % format(string(S), '~w', [Tasks]),
    % reply_json(S).
    %
    sequence_to_dto(Order, DTO),
    %
    reply_json_dict(DTO).
