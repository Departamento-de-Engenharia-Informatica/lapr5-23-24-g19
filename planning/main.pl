% vim: ft=prolog

:- use_module('http-server', [ create_server/1 ]).

main :- create_server.
