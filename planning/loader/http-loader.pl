:- module('http-loader', [
    getmap/3
]).

:- dynamic loaded/3. % loaded(Building, Floor, UNIX_time)

:- use_module(graph, [
    floorcell/4,
    elevator/5,
    passage/6,
    connection/5
]).

% 2 min long cache; adjust as needed
% NOTE: could be configured via environment variable
ttl(120).

already_loaded(Building, Floor) :-
    get_time(Now),
    loaded(Building, Floor, Time),

    ttl(Hold),
    Now - Time =< Hold.


getmap(Building, Floor, Map) :-
    \+ already_loaded(Building, Floor),

    % TODO: http connection
    % 1. Ensure MDR URL is defined in the environment
    %    Check to be added in main.pl
    % 2. Grab MDR URL from environment
    % 3. Map to return should be the equivalent of
    %    JSON.parse(req.body).map

    (
        (retractall(loaded(Building, Floor, _)); true),
        (retractall(floorcell(Building, Floor, _, _)); true),
        (retractall(elevator(Building, Floor, _, _, _)); true),
        (retractall(passage(Building, Floor, _, _, _, _)); true),
        (retractall(connection(Building, Floor, _, _, _)); true)
    ),
    get_time(Now),
    assertz(loaded(Building, Floor, Now)),

    fail.

% vim: ft=prolog
