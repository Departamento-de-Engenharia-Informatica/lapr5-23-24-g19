:- module('http-loader', [
    getmap/3
]).

:- use_module(library(http/http_client)).

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

:- dynamic loaded/4. % loaded(Building, Floor, Cache, UNIX_time)

already_loaded(Building, Floor) :-
    get_time(Now),
    loaded(Building, Floor, _, Time),

    ttl(Hold),
    Now - Time =< Hold.

getmap(Building, Floor, Map) :-
    \+ already_loaded(Building, Floor), !,

    mdr_url(Prefix),
    atomic_list_concat([Prefix, '/buildings/', Building, '/floors/', Floor, '/map'], URL),

    http_get(URL, MapDTO, [content_type('application/json')]),
    Map = MapDTO.map,

    (
        (retractall(loaded(Building, Floor, _, _)), !; true),
        (retractall(floorcell(Building, Floor, _, _)), !; true),
        (retractall(elevator(Building, Floor, _, _, _)), !; true),
        (retractall(passage(Building, Floor, _, _, _, _)), !; true),
        (retractall(connection(Building, Floor, _, _, _)), !; true)
    ),
    get_time(Now),
    assertz(loaded(Building, Floor, Map, Now)),

    fail.

% grab cache if already_loaded
getmap(Building, Floor, Map) :- loaded(Building, Floor, Map, _).

% vim: ft=prolog
