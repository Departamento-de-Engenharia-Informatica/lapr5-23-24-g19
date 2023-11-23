% vim: ft=prolog

:- use_module('http-server').


get_port(Port) :-
    getenv('PLAN_PORT', Val), !,
    atom_number(Val, Port);
    Port is 8090.

loop :- loop.

:- get_port(Port), create_server(Port).

% :-loop.
