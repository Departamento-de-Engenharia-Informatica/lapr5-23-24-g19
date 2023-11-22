% vim: ft=prolog

%% Knowledge base
:- ensure_loaded('knowledge_base').
:- ensure_loaded('algorithms').


:- use_module('http-server').


get_port(Port) :-
    getenv('PLAN_PORT', Val), !,
    atom_number(Val, Port);
    Port is 8090.

:- get_port(Port), create_server(Port).
