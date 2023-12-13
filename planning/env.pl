
:- module(env, [
    loader_ttl/1,
    mdr_url/1,
    elev_cost/1,
    passage_cost/1,
    task_sequence_alg/1
]).


% 2 min long cache; adjust as needed
loader_ttl(120).

mdr_url(Url) :-
    getenv('MDR_PREFIX', Url), !;
    Url = 'http://localhost:4000/api'.
    % Url = 'http://34.231.14.154:4000/api'.

passage_cost(Cost) :-
    getenv('PASSAGE_COST', Cost), !;
    Cost is 5.

elev_cost(Cost) :-
    getenv('ELEV_COST', Cost), !;
    Cost is 30.

task_sequence_alg(Alg) :-
    getenv('TASK_SEQ_ALGORITHM', Alg), !;
    Alg = 'permtutations'.

% vim: ft=prolog
