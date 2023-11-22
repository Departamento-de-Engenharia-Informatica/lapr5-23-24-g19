% vim: ft=prolog

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

:- use_module(library(http/json_convert)).

:- use_module(library(settings)).

:- use_module(library(dicts)).


:- set_setting(http:cors, [*]).

create_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    asserta(port(Port)).

stop_server :-
    retract(port(Port)),
    http_stop_server(Port, _).


criteria([ 'least-elevators', 'least-buildings' ]).

:- http_handler('/api/comm-test', comm_test, [ method(get) ]).

comm_test(_Request) :- format('Status: 200~n~n').

:- http_handler('/api/paths/criteria', path_criteria, [ method(get) ]).

path_criteria(_Request) :-
    criteria(Crit),
    prolog_to_json(Crit, JSON),
    reply_json(JSON).


% should've been a GET
% method(post) does not work
:- http_handler('/api/paths/', get_paths, [method(*)]).

get_paths(Request) :-
    % http_read_json_dict(Request, Dict),
    http_read_json_dict(Request, Data),

    prolog_to_json('mandou', JSON),

    reply_json(JSON).



