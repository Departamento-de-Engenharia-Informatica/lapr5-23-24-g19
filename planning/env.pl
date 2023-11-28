
:- module(env, [
    loader_ttl/1,
    mdr_url/1
]).


% 2 min long cache; adjust as needed
loader_ttl(120).

mdr_url(Url) :-
    getenv('MDR_PREFIX', Url), !;
    Url = 'http://localhost:4000/api'.
    % Url = 'http://34.231.14.154:4000/api'.

% vim: ft=prolog
