:- module('http-loader', [
    getmap/3,
    getrobotpos/3
]).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- use_module(graph, [
    floorcell/4,
    elevator/5,
    passage/6,
    connection/5
]).

:- use_module(env, [
    loader_ttl/1 as ttl,
    mdr_url/1
]).

:- dynamic loaded/5. % loaded(Building, Floor, Cache, RobotPos, UNIX_time)

already_loaded(Building, Floor) :-
    get_time(Now),
    loaded(Building, Floor, _, _, Time),

    ttl(Hold),
    Now - Time =< Hold.

getmap(Building, Floor, Map) :-
    \+ already_loaded(Building, Floor), !,

    mdr_url(Prefix),
    format(string(URL), '~w/buildings/~w/floors/~w/map', [Prefix, Building, Floor]),

    http_open(URL, MapJSON, []),
    json_read_dict(MapJSON, MapDTO),
    close(MapJSON),

    Map = MapDTO.map,
    RobotPos = MapDTO.player.initialPosition,

    (retractall(loaded(Building, Floor, _, _, _)), !; true),
    get_time(Now),
    assertz(loaded(Building, Floor, Map, RobotPos, Now)).

% grab cache if already_loaded
getmap(Building, Floor, Map) :- loaded(Building, Floor, Map, _, _).

getrobotpos(Building, Floor, (Building, Floor, X, Y)) :-
    getmap(Building, Floor, _),
    loaded(Building, Floor, _, [Y,X|_], _).

% vim: ft=prolog
