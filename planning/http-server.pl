% vim: ft=prolog

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(settings)).

:- set_setting(http:cors, [*]).

create_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    asserta(port(Port)).

stop_server :-
    retract(port(Port)),
    http_stop_server(Port, _).


:- http_handler('/api/comm-test', comm_test, [ method(get) ]).

comm_test(_Request) :- format('Status: 200~n~n').





